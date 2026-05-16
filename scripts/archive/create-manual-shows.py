#!/usr/bin/env python3
"""
Create manual shows from shows-data.json into the TDF Social Events calendar.

Usage:
  python3 create-manual-shows.py
"""

import json
import os
import sys
from datetime import datetime, timedelta, timezone

import requests

BASE = "https://tdf-hq.fly.dev"
LOGIN = {"username": "admin", "password": "password123"}


def login() -> str:
    r = requests.post(f"{BASE}/login", json=LOGIN, timeout=15)
    r.raise_for_status()
    return r.json()["token"]


def auth(token: str):
    return {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}


def get_existing(token: str):
    """Fetch existing artists, venues, events (with retry)."""
    artists = {}
    venues = {}
    events = {}
    try:
        r = requests.get(f"{BASE}/social-events/artists?limit=50", headers=auth(token), timeout=30)
        if r.status_code == 200:
            for a in r.json():
                artists[a["artistName"].strip().lower()] = a["artistId"]
    except Exception as e:
        print(f"Warning: could not fetch artists: {e}")

    try:
        r = requests.get(f"{BASE}/social-events/venues?limit=50", headers=auth(token), timeout=30)
        if r.status_code == 200:
            for v in r.json():
                key = f"{v.get('venueName','')}|{v.get('venueCity','')}|{v.get('venueCountry','')}".lower()
                venues[key] = v["venueId"]
    except Exception as e:
        print(f"Warning: could not fetch venues: {e}")

    try:
        r = requests.get(f"{BASE}/social-events/events?limit=20", headers=auth(token), timeout=30)
        if r.status_code == 200:
            for e in r.json():
                events[e["eventTitle"].strip().lower()] = e["eventId"]
    except Exception as e:
        print(f"Warning: could not fetch events: {e}")

    return artists, venues, events


def create_artist(token: str, name: str, bio: str, genres=None):
    r = requests.post(
        f"{BASE}/social-events/artists",
        headers=auth(token),
        json={"artistName": name, "artistBio": bio, "artistGenres": genres or []},
        timeout=15,
    )
    r.raise_for_status()
    return r.json()["artistId"]


def create_venue(token: str, payload):
    r = requests.post(f"{BASE}/social-events/venues", headers=auth(token), json=payload, timeout=15)
    r.raise_for_status()
    return r.json()["venueId"]


def create_event(token: str, payload):
    r = requests.post(f"{BASE}/social-events/events", headers=auth(token), json=payload, timeout=15)
    r.raise_for_status()
    return r.json()["eventId"]


def main():
    shows_path = sys.argv[1] if len(sys.argv) > 1 else "shows-data.json"
    if not os.path.exists(shows_path):
        print(f"Shows data file not found: {shows_path}")
        sys.exit(1)

    with open(shows_path) as f:
        shows = json.load(f)

    print(f"Loaded {len(shows)} show(s) from {shows_path}")
    print("Authenticating...")
    token = login()
    print("Fetching existing data...")
    artists, venues, events = get_existing(token)
    print(f"  Existing artists: {len(artists)}, venues: {len(venues)}, events: {len(events)}")

    stats = {"created_artists": 0, "created_venues": 0, "created_events": 0, "skipped": 0, "errors": 0}

    for show in shows:
        artist_name = show.get("artist_name", "").strip()
        norm = artist_name.lower()
        title = show.get("title", f"{artist_name} Live").strip()

        print(f"\nProcessing: {title}")

        # Skip if event already exists
        if title.lower() in events:
            print(f"  SKIP (already exists): {title}")
            stats["skipped"] += 1
            continue

        # Ensure artist exists
        artist_id = artists.get(norm)
        if not artist_id:
            try:
                artist_id = create_artist(token, artist_name, show.get("description", f"Artist profile for {artist_name}")[:100])
                artists[norm] = artist_id
                stats["created_artists"] += 1
                print(f"  Created artist: {artist_id}")
            except Exception as e:
                print(f"  ERROR creating artist: {e}")
                stats["errors"] += 1
                continue
        else:
            print(f"  Using artist: {artist_id}")

        # Ensure venue exists
        venue_name = show.get("venue_name", "TBD")
        venue_city = show.get("venue_city", "")
        venue_country = show.get("venue_country", "Ecuador")
        vkey = f"{venue_name}|{venue_city}|{venue_country}".lower()

        venue_id = venues.get(vkey)
        if not venue_id:
            try:
                venue_id = create_venue(token, {
                    "venueName": venue_name,
                    "venueAddress": show.get("venue_address", venue_city),
                    "venueCity": venue_city,
                    "venueCountry": venue_country,
                    "venueLat": show.get("venue_lat", 0.0),
                    "venueLng": show.get("venue_lng", 0.0),
                    "venueCapacity": show.get("venue_capacity"),
                })
                venues[vkey] = venue_id
                stats["created_venues"] += 1
                print(f"  Created venue: {venue_id}")
            except Exception as e:
                print(f"  ERROR creating venue: {e}")
                stats["errors"] += 1
                continue
        else:
            print(f"  Using venue: {venue_id}")

        # Parse date
        date_str = show.get("date")
        try:
            start = datetime.fromisoformat(date_str.replace("Z", "+00:00"))
        except (ValueError, AttributeError):
            print(f"  ERROR: invalid date '{date_str}'")
            stats["errors"] += 1
            continue

        end = start + timedelta(hours=show.get("duration_hours", 3))

        # Create event
        try:
            event_id = create_event(token, {
                "eventTitle": title,
                "eventDescription": show.get("description", f"Live performance by {artist_name}"),
                "eventStart": start.strftime("%Y-%m-%dT%H:%M:%SZ"),
                "eventEnd": end.strftime("%Y-%m-%dT%H:%M:%SZ"),
                "eventVenueId": venue_id,
                "eventPriceCents": show.get("price_cents", 0),
                "eventCapacity": show.get("capacity", 200),
                "eventIsPublic": show.get("is_public", True),
                "eventType": show.get("event_type", "concert"),
                "eventStatus": show.get("event_status", "announced"),
                "eventCurrency": show.get("currency", "USD"),
                "eventTicketUrl": show.get("ticket_url"),
                "eventArtists": [{"artistId": artist_id, "artistName": artist_name}],
            })
            events[title.lower()] = event_id
            stats["created_events"] += 1
            print(f"  CREATED event: {event_id} — {title}")
        except Exception as e:
            print(f"  ERROR creating event: {e}")
            stats["errors"] += 1

    print("\n" + "=" * 50)
    print("DONE")
    print("=" * 50)
    for k, v in stats.items():
        print(f"  {k}: {v}")


if __name__ == "__main__":
    main()

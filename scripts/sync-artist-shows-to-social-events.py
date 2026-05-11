#!/usr/bin/env python3
"""
Sync artist shows to the TDF Social Events calendar.

Usage:
  export TDF_API_BASE=https://tdf-hq.fly.dev
  export TDF_USERNAME=admin
  export TDF_PASSWORD=password123
  export BANDSINTOWN_APP_ID=your_app_id      # optional
  export SONGKICK_API_KEY=your_api_key       # optional
  export TICKETMASTER_API_KEY=your_api_key   # optional
  python3 sync-artist-shows-to-social-events.py

If no external concert API key is provided, the script falls back to creating
"Release Showcase" events from each artist's existing releases in the TDF database.
"""

import os
import sys
import json
import requests
from datetime import datetime, timedelta, timezone
from typing import Optional, List, Dict, Any

TDF_API_BASE = os.environ.get("TDF_API_BASE", "https://tdf-hq.fly.dev").rstrip("/")
TDF_USERNAME = os.environ.get("TDF_USERNAME", "admin")
TDF_PASSWORD = os.environ.get("TDF_PASSWORD", "password123")

BANDSINTOWN_APP_ID = os.environ.get("BANDSINTOWN_APP_ID", "")
SONGKICK_API_KEY = os.environ.get("SONGKICK_API_KEY", "")
TICKETMASTER_API_KEY = os.environ.get("TICKETMASTER_API_KEY", "")

session = requests.Session()


def tdf_login() -> str:
    """Authenticate with TDF and return a Bearer token."""
    resp = session.post(
        f"{TDF_API_BASE}/login",
        json={"username": TDF_USERNAME, "password": TDF_PASSWORD},
    )
    resp.raise_for_status()
    return resp.json()["token"]


def tdf_auth_headers(token: str) -> Dict[str, str]:
    return {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}


def get_fan_artists(token: str) -> List[Dict[str, Any]]:
    resp = session.get(f"{TDF_API_BASE}/fans/artists")
    resp.raise_for_status()
    return resp.json()


def get_fan_artist_releases(token: str, artist_id: int) -> List[Dict[str, Any]]:
    resp = session.get(f"{TDF_API_BASE}/fans/artists/{artist_id}/releases")
    resp.raise_for_status()
    return resp.json()


def get_social_artists(token: str) -> List[Dict[str, Any]]:
    resp = session.get(
        f"{TDF_API_BASE}/social-events/artists?limit=1000",
        headers=tdf_auth_headers(token),
    )
    resp.raise_for_status()
    return resp.json()


def get_social_events(token: str) -> List[Dict[str, Any]]:
    resp = session.get(
        f"{TDF_API_BASE}/social-events/events?limit=100",
        headers=tdf_auth_headers(token),
    )
    resp.raise_for_status()
    return resp.json()


def get_venues(token: str) -> List[Dict[str, Any]]:
    resp = session.get(
        f"{TDF_API_BASE}/social-events/venues?limit=100",
        headers=tdf_auth_headers(token),
    )
    resp.raise_for_status()
    return resp.json()


def create_social_artist(token: str, name: str, bio: Optional[str] = None, genres: Optional[List[str]] = None) -> Dict[str, Any]:
    payload = {
        "artistName": name,
        "artistBio": bio or f"Artist profile for {name}",
        "artistGenres": genres or [],
    }
    resp = session.post(
        f"{TDF_API_BASE}/social-events/artists",
        headers=tdf_auth_headers(token),
        json=payload,
    )
    resp.raise_for_status()
    return resp.json()


def create_social_event(token: str, event: Dict[str, Any]) -> Dict[str, Any]:
    resp = session.post(
        f"{TDF_API_BASE}/social-events/events",
        headers=tdf_auth_headers(token),
        json=event,
    )
    resp.raise_for_status()
    return resp.json()


def fetch_bandsintown_shows(artist_name: str) -> List[Dict[str, Any]]:
    if not BANDSINTOWN_APP_ID:
        return []
    try:
        resp = session.get(
            f"https://rest.bandsintown.com/artists/{requests.utils.quote(artist_name)}/events",
            params={"app_id": BANDSINTOWN_APP_ID},
            timeout=10,
        )
        if resp.status_code == 200:
            data = resp.json()
            if isinstance(data, list):
                return data
    except Exception as e:
        print(f"  Bandsintown error for {artist_name}: {e}")
    return []


def fetch_songkick_shows(artist_name: str) -> List[Dict[str, Any]]:
    if not SONGKICK_API_KEY:
        return []
    try:
        resp = session.get(
            "https://api.songkick.com/api/3.0/events.json",
            params={"apikey": SONGKICK_API_KEY, "artist_name": artist_name},
            timeout=10,
        )
        if resp.status_code == 200:
            data = resp.json()
            results = data.get("resultsPage", {}).get("results", {})
            return results.get("event", [])
    except Exception as e:
        print(f"  Songkick error for {artist_name}: {e}")
    return []


def fetch_ticketmaster_shows(artist_name: str) -> List[Dict[str, Any]]:
    if not TICKETMASTER_API_KEY:
        return []
    try:
        resp = session.get(
            "https://app.ticketmaster.com/discovery/v2/events.json",
            params={"apikey": TICKETMASTER_API_KEY, "keyword": artist_name, "classificationName": "music"},
            timeout=10,
        )
        if resp.status_code == 200:
            data = resp.json()
            return data.get("_embedded", {}).get("events", [])
    except Exception as e:
        print(f"  Ticketmaster error for {artist_name}: {e}")
    return []


def normalize_artist_name(name: str) -> str:
    """Case-insensitive comparison key."""
    return name.strip().lower()


def parse_external_date(date_str: str) -> Optional[datetime]:
    """Try to parse various date formats from external APIs."""
    formats = [
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%dT%H:%M:%S%z",
        "%Y-%m-%dT%H:%M:%SZ",
        "%Y-%m-%d",
        "%Y-%m-%d %H:%M:%S",
    ]
    for fmt in formats:
        try:
            return datetime.strptime(date_str, fmt)
        except ValueError:
            continue
    return None


def build_event_from_release(artist_name: str, release: Dict[str, Any], venue_id: str, artist_id: str) -> Optional[Dict[str, Any]]:
    title = release.get("arTitle", "Unknown Release")
    release_date = release.get("arReleaseDate")
    
    if release_date:
        try:
            start = datetime.strptime(release_date, "%Y-%m-%d").replace(hour=20, minute=0, second=0, tzinfo=timezone.utc)
        except ValueError:
            start = datetime.now(timezone.utc) + timedelta(days=30)
    else:
        start = datetime.now(timezone.utc) + timedelta(days=30)
    
    end = start + timedelta(hours=3)
    
    return {
        "eventTitle": f"{artist_name} — {title} (Release Showcase)",
        "eventDescription": f"Release showcase for '{title}' by {artist_name}.",
        "eventStart": start.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "eventEnd": end.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "eventVenueId": venue_id,
        "eventPriceCents": 0,
        "eventCapacity": 200,
        "eventIsPublic": True,
        "eventType": "showcase",
        "eventStatus": "announced",
        "eventCurrency": "USD",
        "eventArtists": [{"artistId": artist_id, "artistName": artist_name}],
    }


def build_event_from_external_show(artist_name: str, show: Dict[str, Any], venue_id: str, artist_id: str) -> Optional[Dict[str, Any]]:
    # Bandsintown format
    if "datetime" in show or "starts_at" in show:
        title = show.get("title") or show.get("name") or f"{artist_name} Live"
        venue_data = show.get("venue", {})
        venue_name = venue_data.get("name", "TBD")
        city = venue_data.get("city", "")
        country = venue_data.get("country", "")
        
        date_str = show.get("datetime") or show.get("starts_at") or show.get("start", {}).get("date")
        if not date_str:
            return None
        
        start = parse_external_date(date_str)
        if not start:
            return None
        
        if start.tzinfo is None:
            start = start.replace(tzinfo=timezone.utc)
        end = start + timedelta(hours=3)
        
        description = f"Live performance by {artist_name}"
        if city:
            description += f" in {city}"
        if country:
            description += f", {country}"
        
        return {
            "eventTitle": title,
            "eventDescription": description,
            "eventStart": start.strftime("%Y-%m-%dT%H:%M:%SZ"),
            "eventEnd": end.strftime("%Y-%m-%dT%H:%M:%SZ"),
            "eventVenueId": venue_id,
            "eventPriceCents": 0,
            "eventCapacity": 200,
            "eventIsPublic": True,
            "eventType": "concert",
            "eventStatus": "announced",
            "eventCurrency": "USD",
            "eventArtists": [{"artistId": artist_id, "artistName": artist_name}],
        }
    
    # Ticketmaster format
    if "dates" in show:
        title = show.get("name", f"{artist_name} Live")
        dates = show.get("dates", {})
        start_data = dates.get("start", {})
        date_str = start_data.get("dateTime") or start_data.get("localDate")
        if not date_str:
            return None
        
        start = parse_external_date(date_str)
        if not start:
            return None
        if start.tzinfo is None:
            start = start.replace(tzinfo=timezone.utc)
        end = start + timedelta(hours=3)
        
        venue_data = show.get("_embedded", {}).get("venues", [{}])[0]
        city = venue_data.get("city", {}).get("name", "")
        
        return {
            "eventTitle": title,
            "eventDescription": f"Live performance by {artist_name}" + (f" in {city}" if city else ""),
            "eventStart": start.strftime("%Y-%m-%dT%H:%M:%SZ"),
            "eventEnd": end.strftime("%Y-%m-%dT%H:%M:%SZ"),
            "eventVenueId": venue_id,
            "eventPriceCents": 0,
            "eventCapacity": 200,
            "eventIsPublic": True,
            "eventType": "concert",
            "eventStatus": "announced",
            "eventCurrency": "USD",
            "eventArtists": [{"artistId": artist_id, "artistName": artist_name}],
        }
    
    return None


def main():
    print("=" * 60)
    print("TDF Artist Shows → Social Events Sync")
    print("=" * 60)
    print(f"API Base: {TDF_API_BASE}")
    print(f"External APIs configured:")
    print(f"  Bandsintown: {'YES' if BANDSINTOWN_APP_ID else 'NO'}")
    print(f"  Songkick:    {'YES' if SONGKICK_API_KEY else 'NO'}")
    print(f"  Ticketmaster:{'YES' if TICKETMASTER_API_KEY else 'NO'}")
    if not any([BANDSINTOWN_APP_ID, SONGKICK_API_KEY, TICKETMASTER_API_KEY]):
        print("\nNo external concert API keys found.")
        print("Falling back to creating Release Showcase events from TDF releases.\n")
    
    print("\nAuthenticating with TDF...")
    token = tdf_login()
    print("Authenticated successfully.\n")
    
    print("Fetching fan artists...")
    fan_artists = get_fan_artists(token)
    print(f"Found {len(fan_artists)} fan artists.\n")
    
    print("Fetching existing social event artists...")
    social_artists = get_social_artists(token)
    social_artist_by_name = {normalize_artist_name(a["artistName"]): a for a in social_artists}
    print(f"Found {len(social_artists)} existing social event artists.\n")
    
    print("Fetching existing social events...")
    existing_events = get_social_events(token)
    existing_event_titles = {e["eventTitle"].lower() for e in existing_events}
    print(f"Found {len(existing_events)} existing social events.\n")
    
    print("Fetching venues...")
    venues = get_venues(token)
    if venues:
        default_venue = venues[0]
        venue_id = default_venue["venueId"]
        print(f"Using venue: {default_venue['venueName']} (ID: {venue_id})\n")
    else:
        print("No venues found! Please create a venue first.")
        sys.exit(1)
    
    artists_processed = 0
    events_created = 0
    artists_created = 0
    errors = 0
    
    for fan_artist in fan_artists:
        artist_id = fan_artist["apArtistId"]
        artist_name = fan_artist["apDisplayName"]
        print(f"Processing: {artist_name} (ID: {artist_id})")
        
        # Ensure social event artist exists
        norm_name = normalize_artist_name(artist_name)
        social_artist = social_artist_by_name.get(norm_name)
        if not social_artist:
            try:
                social_artist = create_social_artist(
                    token,
                    name=artist_name,
                    bio=fan_artist.get("apBio") or f"Artist profile for {artist_name}",
                    genres=fan_artist.get("apGenres") or [],
                )
                social_artist_by_name[norm_name] = social_artist
                artists_created += 1
                print(f"  Created social artist: {social_artist['artistId']}")
            except Exception as e:
                print(f"  ERROR creating social artist: {e}")
                errors += 1
                continue
        else:
            print(f"  Using existing social artist: {social_artist['artistId']}")
        
        social_artist_id = social_artist["artistId"]
        shows_found: List[Dict[str, Any]] = []
        
        # Try external APIs
        if BANDSINTOWN_APP_ID:
            shows_found.extend(fetch_bandsintown_shows(artist_name))
        if SONGKICK_API_KEY:
            shows_found.extend(fetch_songkick_shows(artist_name))
        if TICKETMASTER_API_KEY:
            shows_found.extend(fetch_ticketmaster_shows(artist_name))
        
        # Fallback to releases if no external shows found
        if not shows_found:
            releases = get_fan_artist_releases(token, artist_id)
            if releases:
                print(f"  No external shows found. Using {len(releases)} release(s) as fallback.")
                for release in releases:
                    event = build_event_from_release(artist_name, release, venue_id, social_artist_id)
                    if event:
                        shows_found.append({"__tdf_fallback__": True, "__event__": event})
            else:
                print(f"  No external shows and no releases. Skipping.")
                continue
        else:
            print(f"  Found {len(shows_found)} show(s) from external API(s).")
        
        for show in shows_found:
            try:
                if show.get("__tdf_fallback__"):
                    event_payload = show["__event__"]
                else:
                    event_payload = build_event_from_external_show(artist_name, show, venue_id, social_artist_id)
                
                if not event_payload:
                    continue
                
                # Skip duplicates by title
                if event_payload["eventTitle"].lower() in existing_event_titles:
                    print(f"  SKIP (duplicate): {event_payload['eventTitle']}")
                    continue
                
                created = create_social_event(token, event_payload)
                existing_event_titles.add(event_payload["eventTitle"].lower())
                events_created += 1
                print(f"  CREATED event: {created['eventId']} — {created['eventTitle']}")
            except Exception as e:
                print(f"  ERROR creating event: {e}")
                errors += 1
        
        artists_processed += 1
        print()
    
    print("=" * 60)
    print("SYNC COMPLETE")
    print("=" * 60)
    print(f"Artists processed: {artists_processed}")
    print(f"Social artists created: {artists_created}")
    print(f"Events created: {events_created}")
    print(f"Errors: {errors}")


if __name__ == "__main__":
    main()

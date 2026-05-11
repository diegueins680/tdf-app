#!/usr/bin/env python3
"""
TDF Artist Shows Sync — with Bandsintown + Songkick + Manual Input

Usage:
  # With external APIs (recommended)
  export BANDSINTOWN_APP_ID=your_app_id
  export SONGKICK_API_KEY=your_api_key
  python3 sync-concerts-with-apis.py

  # With manual show data only (no APIs)
  python3 sync-concerts-with-apis.py --manual-only

  # With a custom manual shows JSON file
  python3 sync-concerts-with-apis.py --manual-shows shows-data.json

Getting API Keys:
  - Bandsintown: https://artists.bandsintown.com/support/api-installation
  - Songkick:    https://www.songkick.com/api (free tier available)
"""

import argparse
import json
import os
import sys
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, List, Optional

import requests

TDF_API_BASE = os.environ.get("TDF_API_BASE", "https://tdf-hq.fly.dev").rstrip("/")
TDF_USERNAME = os.environ.get("TDF_USERNAME", "admin")
TDF_PASSWORD = os.environ.get("TDF_PASSWORD", "password123")

BANDSINTOWN_APP_ID = os.environ.get("BANDSINTOWN_APP_ID", "").strip()
SONGKICK_API_KEY = os.environ.get("SONGKICK_API_KEY", "").strip()

session = requests.Session()


# ═══════════════════════════════════════════════════════════════════════════════
# TDF API helpers
# ═══════════════════════════════════════════════════════════════════════════════

def tdf_login() -> str:
    resp = session.post(
        f"{TDF_API_BASE}/login",
        json={"username": TDF_USERNAME, "password": TDF_PASSWORD},
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json()["token"]


def tdf_auth(token: str) -> Dict[str, str]:
    return {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}


def get_fan_artists(token: str) -> List[Dict[str, Any]]:
    resp = session.get(f"{TDF_API_BASE}/fans/artists", timeout=15)
    resp.raise_for_status()
    return resp.json()


def get_social_artists(token: str) -> List[Dict[str, Any]]:
    resp = session.get(
        f"{TDF_API_BASE}/social-events/artists?limit=50",
        headers=tdf_auth(token),
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json()


def get_social_events(token: str) -> List[Dict[str, Any]]:
    resp = session.get(
        f"{TDF_API_BASE}/social-events/events?limit=50",
        headers=tdf_auth(token),
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json()


def get_venues(token: str) -> List[Dict[str, Any]]:
    resp = session.get(
        f"{TDF_API_BASE}/social-events/venues?limit=50",
        headers=tdf_auth(token),
        timeout=30,
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
        headers=tdf_auth(token),
        json=payload,
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json()


def create_venue(token: str, payload: Dict[str, Any]) -> Dict[str, Any]:
    resp = session.post(
        f"{TDF_API_BASE}/social-events/venues",
        headers=tdf_auth(token),
        json=payload,
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json()


def create_event(token: str, payload: Dict[str, Any]) -> Dict[str, Any]:
    resp = session.post(
        f"{TDF_API_BASE}/social-events/events",
        headers=tdf_auth(token),
        json=payload,
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json()


# ═══════════════════════════════════════════════════════════════════════════════
# External API fetchers
# ═══════════════════════════════════════════════════════════════════════════════

def fetch_bandsintown_shows(artist_name: str) -> List[Dict[str, Any]]:
    if not BANDSINTOWN_APP_ID:
        return []
    try:
        resp = session.get(
            f"https://rest.bandsintown.com/artists/{requests.utils.quote(artist_name)}/events",
            params={"app_id": BANDSINTOWN_APP_ID},
            timeout=30,
        )
        if resp.status_code == 200:
            data = resp.json()
            if isinstance(data, list):
                # Filter for future shows only
                now = datetime.now(timezone.utc)
                future = []
                for show in data:
                    dt_str = show.get("datetime") or show.get("starts_at")
                    if dt_str:
                        try:
                            dt = datetime.fromisoformat(dt_str.replace("Z", "+00:00"))
                            if dt >= now:
                                future.append(show)
                        except ValueError:
                            future.append(show)
                    else:
                        future.append(show)
                return future
    except Exception as e:
        print(f"  [Bandsintown] Error for '{artist_name}': {e}")
    return []


def fetch_songkick_shows(artist_name: str) -> List[Dict[str, Any]]:
    if not SONGKICK_API_KEY:
        return []
    try:
        resp = session.get(
            "https://api.songkick.com/api/3.0/events.json",
            params={"apikey": SONGKICK_API_KEY, "artist_name": artist_name},
            timeout=30,
        )
        if resp.status_code == 200:
            data = resp.json()
            results = data.get("resultsPage", {}).get("results", {})
            events = results.get("event", [])
            # Filter future
            now = datetime.now(timezone.utc)
            future = []
            for ev in events:
                start = ev.get("start", {})
                date_str = start.get("datetime") or start.get("date")
                if date_str:
                    try:
                        if "T" in date_str:
                            dt = datetime.fromisoformat(date_str.replace("Z", "+00:00"))
                        else:
                            dt = datetime.strptime(date_str, "%Y-%m-%d").replace(tzinfo=timezone.utc)
                        if dt >= now:
                            future.append(ev)
                    except ValueError:
                        future.append(ev)
                else:
                    future.append(ev)
            return future
    except Exception as e:
        print(f"  [Songkick] Error for '{artist_name}': {e}")
    return []


# ═══════════════════════════════════════════════════════════════════════════════
# Transform external shows → TDF event payloads
# ═══════════════════════════════════════════════════════════════════════════════

def parse_iso_datetime(date_str: str) -> Optional[datetime]:
    if not date_str:
        return None
    try:
        return datetime.fromisoformat(date_str.replace("Z", "+00:00"))
    except ValueError:
        pass
    try:
        return datetime.strptime(date_str, "%Y-%m-%d").replace(tzinfo=timezone.utc)
    except ValueError:
        pass
    return None


def build_event_from_bandsintown(show: Dict[str, Any], artist_name: str, artist_id: str, venue_cache: Dict[str, str]) -> Optional[Dict[str, Any]]:
    title = show.get("title") or show.get("name") or f"{artist_name} Live"
    venue_data = show.get("venue", {})
    venue_name = venue_data.get("name", "TBD")
    city = venue_data.get("city", "")
    country = venue_data.get("country", "")
    region = venue_data.get("region", "")
    lat = venue_data.get("latitude")
    lng = venue_data.get("longitude")

    date_str = show.get("datetime") or show.get("starts_at")
    start = parse_iso_datetime(date_str)
    if not start:
        return None

    end = start + timedelta(hours=3)

    description = f"Live performance by {artist_name}"
    if city:
        description += f" in {city}"
    if country:
        description += f", {country}"
    if show.get("url"):
        description += f"\n\nMore info: {show['url']}"

    # Find or create venue
    venue_key = f"{venue_name}|{city}|{country}".lower()
    venue_id = venue_cache.get(venue_key)

    return {
        "__venue_key__": venue_key,
        "__venue_payload__": {
            "venueName": venue_name,
            "venueAddress": f"{city}, {region}".strip(", "),
            "venueCity": city,
            "venueCountry": country,
            "venueLat": float(lat) if lat else 0.0,
            "venueLng": float(lng) if lng else 0.0,
        } if not venue_id else None,
        "eventTitle": title,
        "eventDescription": description,
        "eventStart": start.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "eventEnd": end.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "eventVenueId": venue_id or "PENDING",
        "eventPriceCents": 0,
        "eventCapacity": 200,
        "eventIsPublic": True,
        "eventType": "concert",
        "eventStatus": "announced",
        "eventCurrency": "USD",
        "eventTicketUrl": show.get("url") or show.get("ticket_url") or None,
        "eventArtists": [{"artistId": artist_id, "artistName": artist_name}],
    }


def build_event_from_songkick(ev: Dict[str, Any], artist_name: str, artist_id: str, venue_cache: Dict[str, str]) -> Optional[Dict[str, Any]]:
    title = ev.get("displayName") or f"{artist_name} Live"
    venue_data = ev.get("venue", {})
    venue_name = venue_data.get("displayName", "TBD")
    city_data = ev.get("location", {}).get("city", "")
    lat = ev.get("venue", {}).get("lat")
    lng = ev.get("venue", {}).get("lng")

    start_data = ev.get("start", {})
    date_str = start_data.get("datetime") or start_data.get("date")
    start = parse_iso_datetime(date_str)
    if not start:
        return None

    end = start + timedelta(hours=3)

    description = f"Live performance by {artist_name}"
    if city_data:
        description += f" in {city_data}"

    venue_key = f"{venue_name}|{city_data}|Ecuador".lower()
    venue_id = venue_cache.get(venue_key)

    return {
        "__venue_key__": venue_key,
        "__venue_payload__": {
            "venueName": venue_name,
            "venueAddress": city_data,
            "venueCity": city_data.split(",")[0].strip() if "," in city_data else city_data,
            "venueCountry": "Ecuador",
            "venueLat": float(lat) if lat else 0.0,
            "venueLng": float(lng) if lng else 0.0,
        } if not venue_id else None,
        "eventTitle": title,
        "eventDescription": description,
        "eventStart": start.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "eventEnd": end.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "eventVenueId": venue_id or "PENDING",
        "eventPriceCents": 0,
        "eventCapacity": 200,
        "eventIsPublic": True,
        "eventType": "concert",
        "eventStatus": "announced",
        "eventCurrency": "USD",
        "eventTicketUrl": ev.get("uri"),
        "eventArtists": [{"artistId": artist_id, "artistName": artist_name}],
    }


# ═══════════════════════════════════════════════════════════════════════════════
# Manual show data support
# ═══════════════════════════════════════════════════════════════════════════════

def load_manual_shows(path: str) -> List[Dict[str, Any]]:
    with open(path) as f:
        data = json.load(f)
    if isinstance(data, list):
        return data
    return data.get("shows", [])


def build_event_from_manual(show: Dict[str, Any], artist_name: str, artist_id: str, venue_cache: Dict[str, str]) -> Optional[Dict[str, Any]]:
    title = show.get("title") or f"{artist_name} Live"
    date_str = show.get("date") or show.get("start")
    start = parse_iso_datetime(date_str)
    if not start:
        return None
    end = start + timedelta(hours=show.get("duration_hours", 3))

    venue_name = show.get("venue_name", "TBD")
    venue_city = show.get("venue_city", "")
    venue_country = show.get("venue_country", "Ecuador")
    venue_address = show.get("venue_address", venue_city)

    venue_key = f"{venue_name}|{venue_city}|{venue_country}".lower()
    venue_id = venue_cache.get(venue_key)

    return {
        "__venue_key__": venue_key,
        "__venue_payload__": {
            "venueName": venue_name,
            "venueAddress": venue_address,
            "venueCity": venue_city,
            "venueCountry": venue_country,
            "venueLat": show.get("venue_lat", 0.0),
            "venueLng": show.get("venue_lng", 0.0),
            "venueCapacity": show.get("venue_capacity"),
        } if not venue_id else None,
        "eventTitle": title,
        "eventDescription": show.get("description", f"Live performance by {artist_name}"),
        "eventStart": start.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "eventEnd": end.strftime("%Y-%m-%dT%H:%M:%SZ"),
        "eventVenueId": venue_id or "PENDING",
        "eventPriceCents": show.get("price_cents", 0),
        "eventCapacity": show.get("capacity", 200),
        "eventIsPublic": show.get("is_public", True),
        "eventType": show.get("event_type", "concert"),
        "eventStatus": show.get("event_status", "announced"),
        "eventCurrency": show.get("currency", "USD"),
        "eventTicketUrl": show.get("ticket_url"),
        "eventArtists": [{"artistId": artist_id, "artistName": artist_name}],
    }


# ═══════════════════════════════════════════════════════════════════════════════
# Main sync logic
# ═══════════════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(description="Sync artist shows to TDF Social Events")
    parser.add_argument("--manual-only", action="store_true", help="Only use manual show data, skip APIs")
    parser.add_argument("--manual-shows", type=str, default="shows-data.json", help="Path to manual shows JSON")
    parser.add_argument("--dry-run", action="store_true", help="Preview what would be created without writing")
    args = parser.parse_args()

    print("=" * 70)
    print("TDF Artist Shows → Social Events Sync (v2 with APIs)")
    print("=" * 70)
    print(f"TDF API:    {TDF_API_BASE}")
    print(f"Bandsintown API: {'YES (' + BANDSINTOWN_APP_ID[:8] + '...)' if BANDSINTOWN_APP_ID else 'NO (set BANDSINTOWN_APP_ID env var)'}")
    print(f"Songkick API:    {'YES (' + SONGKICK_API_KEY[:8] + '...)' if SONGKICK_API_KEY else 'NO (set SONGKICK_API_KEY env var)'}")
    print(f"Manual shows:    {args.manual_shows if os.path.exists(args.manual_shows) else 'NOT FOUND'}")
    print(f"Dry run:         {args.dry_run}")
    print()

    # Auth
    print("Authenticating with TDF...")
    token = tdf_login()
    print("Authenticated.\n")

    # Load data
    fan_artists = get_fan_artists(token)
    social_artists = get_social_artists(token)
    existing_events = get_social_events(token)
    existing_venues = get_venues(token)

    # Build lookup maps
    social_artist_by_name = {a["artistName"].strip().lower(): a for a in social_artists}
    existing_event_titles = {e["eventTitle"].lower().strip(): e["eventId"] for e in existing_events}
    venue_cache = {}
    for v in existing_venues:
        key = f"{v.get('venueName','')}|{v.get('venueCity','')}|{v.get('venueCountry','')}".lower()
        venue_cache[key] = v["venueId"]

    # Load manual shows
    manual_shows_map: Dict[str, List[Dict[str, Any]]] = {}
    if os.path.exists(args.manual_shows):
        print(f"Loading manual shows from {args.manual_shows}...")
        all_manual = load_manual_shows(args.manual_shows)
        for show in all_manual:
            name = show.get("artist_name", "").strip().lower()
            if name:
                manual_shows_map.setdefault(name, []).append(show)
        print(f"  Loaded {len(all_manual)} manual show(s) for {len(manual_shows_map)} artist(s).\n")

    stats = {"artists": 0, "artists_created": 0, "venues_created": 0, "events_created": 0, "errors": 0, "skipped": 0}

    for fan_artist in fan_artists:
        artist_id = fan_artist["apArtistId"]
        artist_name = fan_artist["apDisplayName"]
        norm_name = artist_name.strip().lower()

        print(f"Processing: {artist_name} (fan ID: {artist_id})")

        # Ensure social artist exists
        social_artist = social_artist_by_name.get(norm_name)
        if not social_artist:
            if args.dry_run:
                print(f"  [DRY-RUN] Would create social artist: {artist_name}")
                social_artist_id = "DRY"
            else:
                try:
                    social_artist = create_social_artist(
                        token,
                        name=artist_name,
                        bio=fan_artist.get("apBio") or f"Artist profile for {artist_name}",
                        genres=fan_artist.get("apGenres") or [],
                    )
                    social_artist_by_name[norm_name] = social_artist
                    stats["artists_created"] += 1
                    print(f"  Created social artist: {social_artist['artistId']}")
                except Exception as e:
                    print(f"  ERROR creating artist: {e}")
                    stats["errors"] += 1
                    continue
            social_artist_id = social_artist["artistId"]
        else:
            social_artist_id = social_artist["artistId"]
            print(f"  Using existing social artist: {social_artist_id}")

        # Collect shows from all sources
        all_shows: List[Dict[str, Any]] = []
        source_labels: List[str] = []

        if not args.manual_only:
            # Try Bandsintown
            if BANDSINTOWN_APP_ID:
                shows = fetch_bandsintown_shows(artist_name)
                for s in shows:
                    all_shows.append(("bandsintown", s))
                if shows:
                    print(f"  Bandsintown: {len(shows)} future show(s)")

            # Try Songkick
            if SONGKICK_API_KEY:
                shows = fetch_songkick_shows(artist_name)
                for s in shows:
                    all_shows.append(("songkick", s))
                if shows:
                    print(f"  Songkick: {len(shows)} future show(s)")

        # Try manual shows
        manual = manual_shows_map.get(norm_name, [])
        for s in manual:
            all_shows.append(("manual", s))
        if manual:
            print(f"  Manual: {len(manual)} show(s)")

        if not all_shows:
            print(f"  No shows found for {artist_name}.\n")
            continue

        # Process shows
        for source, show in all_shows:
            try:
                if source == "bandsintown":
                    event = build_event_from_bandsintown(show, artist_name, social_artist_id, venue_cache)
                elif source == "songkick":
                    event = build_event_from_songkick(show, artist_name, social_artist_id, venue_cache)
                else:
                    event = build_event_from_manual(show, artist_name, social_artist_id, venue_cache)

                if not event:
                    continue

                title = event["eventTitle"].lower().strip()
                if title in existing_event_titles:
                    print(f"  SKIP (duplicate): {event['eventTitle']}")
                    stats["skipped"] += 1
                    continue

                if args.dry_run:
                    print(f"  [DRY-RUN] Would create event: {event['eventTitle']} @ {event['eventStart']}")
                    stats["events_created"] += 1
                    existing_event_titles[title] = "DRY"
                    continue

                # Create venue if needed
                venue_payload = event.pop("__venue_payload__", None)
                venue_key = event.pop("__venue_key__", None)
                if venue_payload and venue_key:
                    try:
                        v = create_venue(token, venue_payload)
                        venue_cache[venue_key] = v["venueId"]
                        event["eventVenueId"] = v["venueId"]
                        stats["venues_created"] += 1
                        print(f"  Created venue: {v['venueName']} (ID: {v['venueId']})")
                    except Exception as e:
                        print(f"  ERROR creating venue: {e}")
                        stats["errors"] += 1
                        continue

                created = create_event(token, event)
                existing_event_titles[title] = created["eventId"]
                stats["events_created"] += 1
                print(f"  CREATED event: {created['eventId']} — {created['eventTitle']} ({source})")

            except Exception as e:
                print(f"  ERROR creating event: {e}")
                stats["errors"] += 1

        stats["artists"] += 1
        print()

    print("=" * 70)
    print("SYNC COMPLETE")
    print("=" * 70)
    for k, v in stats.items():
        print(f"  {k}: {v}")
    print()
    if not BANDSINTOWN_APP_ID and not SONGKICK_API_KEY and not manual_shows_map:
        print("No external APIs configured and no manual show data found.")
        print("To get real concert data, either:")
        print("  1. Set BANDSINTOWN_APP_ID and/or SONGKICK_API_KEY env vars")
        print("  2. Create a shows-data.json file with manual show data")
        print()
        print("Example shows-data.json:")
        print(json.dumps([
            {
                "artist_name": "Arkabuz",
                "title": "Arkabuz - Esencia Eterna Tour",
                "date": "2026-08-15T20:00:00-05:00",
                "venue_name": "La Cuadra - Centro Historico",
                "venue_city": "Quito",
                "venue_country": "Ecuador",
                "description": "Arkabuz en concierto presentando Esencia Eterna",
                "ticket_url": "https://buenplan.com.ec"
            }
        ], indent=2))


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Change all La Cuadra events to Domo del Pululahua venue."""

import os
import sys
import requests

BASE = os.environ.get("TDF_API_BASE", "https://tdf-hq.fly.dev")
LOGIN = {
    "username": os.environ.get("TDF_ADMIN_USER", ""),
    "password": os.environ.get("TDF_ADMIN_PASSWORD", ""),
}
DOMO_VENUE_ID = os.environ.get("DOMO_VENUE_ID", "7")
LA_CUADRA_IDS = set(os.environ.get("LA_CUADRA_IDS", "1,6").split(","))
DRY_RUN = os.environ.get("DRY_RUN", "").lower() in ("1", "true", "yes")


def login():
    if not LOGIN["username"] or not LOGIN["password"]:
        print("ERROR: Set TDF_ADMIN_USER and TDF_ADMIN_PASSWORD environment variables.", file=sys.stderr)
        sys.exit(1)
    r = requests.post(f"{BASE}/login", json=LOGIN, timeout=15)
    r.raise_for_status()
    return r.json()["token"]


def auth(token):
    return {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}


def get_all_events(token):
    events = []
    for offset in range(0, 1000, 50):
        r = requests.get(
            f"{BASE}/social-events/events?limit=50&offset={offset}",
            headers=auth(token),
            timeout=30,
        )
        d = r.json()
        events.extend(d)
        if len(d) == 0:
            break
    return events


def update_event(token, event):
    event_id = event["eventId"]

    # Replace La Cuadra in title
    title = event["eventTitle"]
    if "La Cuadra" in title:
        title = title.replace("La Cuadra", "Domo del Pululahua")

    # Replace La Cuadra in description
    desc = event.get("eventDescription") or ""
    if "La Cuadra" in desc:
        desc = desc.replace("La Cuadra", "Domo del Pululahua")

    # Build minimal update payload
    update = {
        "eventTitle": title,
        "eventDescription": desc if desc else None,
        "eventVenueId": DOMO_VENUE_ID,
        "eventStart": event["eventStart"],
        "eventEnd": event["eventEnd"],
        "eventPriceCents": event.get("eventPriceCents", 0),
        "eventCapacity": event.get("eventCapacity"),
        "eventIsPublic": event.get("eventIsPublic", True),
        "eventType": event.get("eventType", "concert"),
        "eventStatus": event.get("eventStatus", "announced"),
        "eventCurrency": event.get("eventCurrency", "USD"),
        "eventTicketUrl": event.get("eventTicketUrl"),
        "eventArtists": [
            {"artistId": a["artistId"], "artistName": a["artistName"]}
            for a in event.get("eventArtists", [])
        ],
    }

    if DRY_RUN:
        return 200, "dry-run"

    r = requests.put(
        f"{BASE}/social-events/events/{event_id}",
        headers=auth(token),
        json=update,
        timeout=30,
    )
    return r.status_code, r.text


def main():
    if DRY_RUN:
        print("=== DRY RUN mode — no changes will be written ===\n")
    elif "--confirm" not in sys.argv:
        print("ERROR: This script modifies production event data.", file=sys.stderr)
        print("Run with --confirm to execute writes, or set DRY_RUN=1 to preview.", file=sys.stderr)
        sys.exit(1)

    print("Authenticating...")
    token = login()
    print("Fetching all events...")
    events = get_all_events(token)
    print(f"Total events: {len(events)}")

    cuadra_events = [e for e in events if e.get("eventVenueId") in LA_CUADRA_IDS]
    print(f"Events at La Cuadra: {len(cuadra_events)}\n")

    updated = 0
    errors = 0

    for i, e in enumerate(cuadra_events, 1):
        print(f"[{i}/{len(cuadra_events)}] Updating event {e['eventId']}: {e['eventTitle'][:60]}...")
        try:
            status, text = update_event(token, e)
            if status == 200:
                updated += 1
                print(f"  -> OK (venue changed to Domo del Pululahua)")
            else:
                errors += 1
                print(f"  -> ERROR {status}: {text[:200]}")
        except Exception as ex:
            errors += 1
            print(f"  -> EXCEPTION: {ex}")

    print(f"\n{'=' * 50}")
    print(f"Updated: {updated}")
    print(f"Errors:  {errors}")
    print(f"Total:   {len(cuadra_events)}")


if __name__ == "__main__":
    main()

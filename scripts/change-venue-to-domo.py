#!/usr/bin/env python3
"""Change all La Cuadra events to Domo del Pululahua venue."""

import requests

BASE = "https://tdf-hq.fly.dev"
LOGIN = {"username": "admin", "password": "password123"}
DOMO_VENUE_ID = "7"
LA_CUADRA_IDS = {"1", "6"}


def login():
    r = requests.post(f"{BASE}/login", json=LOGIN, timeout=15)
    r.raise_for_status()
    return r.json()["token"]


def auth(token):
    return {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}


def get_all_events(token):
    events = []
    for offset in range(0, 100, 10):
        r = requests.get(
            f"{BASE}/social-events/events?limit=10&offset={offset}",
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

    r = requests.put(
        f"{BASE}/social-events/events/{event_id}",
        headers=auth(token),
        json=update,
        timeout=30,
    )
    return r.status_code, r.text


def main():
    print("Authenticating...")
    token = login()
    print("Fetching all events...")
    events = get_all_events(token)
    print(f"Total events: {len(events)}")

    cuadra_events = [e for e in events if e.get("eventVenueId") in LA_CUADRA_IDS]
    print(f"Events at La Cuadra: {len(cuadra_events)}\n")

    updated = 0
    errors = 0
    skipped = 0

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

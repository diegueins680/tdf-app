#!/usr/bin/env python3
"""
Create confirmed Verde 70 concerts in the TDF Social Events calendar.
Based on research from Primicias.ec (March 2026):
- June 4, 2026 - Ágora Casa de la Cultura, Quito
- June 6, 2026 - Arena Park, Samborondón
"""

import requests
import json

BASE = "https://tdf-hq.fly.dev"
TOKEN = "10c05a7a-6217-423f-822f-61d615b90388"
HEADERS = {
    "Authorization": f"Bearer {TOKEN}",
    "Content-Type": "application/json",
}


def create_artist(name, bio, genres=None):
    payload = {
        "artistName": name,
        "artistBio": bio,
        "artistGenres": genres or [],
    }
    resp = requests.post(f"{BASE}/social-events/artists", headers=HEADERS, json=payload, timeout=15)
    resp.raise_for_status()
    return resp.json()


def create_venue(name, address, city, country, lat, lng, capacity=None):
    payload = {
        "venueName": name,
        "venueAddress": address,
        "venueCity": city,
        "venueCountry": country,
        "venueLat": lat,
        "venueLng": lng,
        "venueCapacity": capacity,
    }
    resp = requests.post(f"{BASE}/social-events/venues", headers=HEADERS, json=payload, timeout=15)
    resp.raise_for_status()
    return resp.json()


def create_event(event):
    resp = requests.post(f"{BASE}/social-events/events", headers=HEADERS, json=event, timeout=15)
    resp.raise_for_status()
    return resp.json()


def main():
    print("Creating Verde 70 events...")
    
    # 1. Create Verde 70 social artist
    print("\n1. Creating Verde 70 social artist...")
    artist = create_artist(
        name="Verde 70",
        bio="Icónico grupo ecuatoriano de rock pop con más de 20 años de trayectoria. Presentan su nuevo álbum 'Fulgor'.",
        genres=["rock", "pop", "electro pop", "indie rock"],
    )
    print(f"   Created artist ID: {artist['artistId']}")
    artist_id = artist["artistId"]
    
    # 2. Create venues
    print("\n2. Creating venues...")
    
    venue_quito = create_venue(
        name="Ágora Casa de la Cultura",
        address="Av. 6 de Diciembre y Patria, Quito",
        city="Quito",
        country="Ecuador",
        lat=-0.2095,
        lng=-78.4978,
        capacity=1500,
    )
    print(f"   Created Quito venue ID: {venue_quito['venueId']}")
    
    venue_gye = create_venue(
        name="Arena Park",
        address="Samborondón, Guayaquil",
        city="Samborondón",
        country="Ecuador",
        lat=-2.1234,
        lng=-79.8765,
        capacity=3000,
    )
    print(f"   Created Samborondón venue ID: {venue_gye['venueId']}")
    
    # 3. Create events
    print("\n3. Creating events...")
    
    event_quito = create_event({
        "eventTitle": "Verde 70 - Gira Fulgor",
        "eventDescription": "Concierto de Verde 70 presentando su nuevo álbum 'Fulgor' en Quito. Incluye clásicos como 'Me tienes, me puedes, dueles', 'Palabras' y 'En la inmensidad', junto a nuevas canciones con electro pop, huayno andino, bolero, cumbia y tango.\n\nFecha: 4 de junio de 2026\nHora: 20:00\nLugar: Ágora Casa de la Cultura, Quito\n\nEntradas disponibles en TicketShow.",
        "eventStart": "2026-06-04T20:00:00-05:00",
        "eventEnd": "2026-06-04T23:00:00-05:00",
        "eventVenueId": venue_quito["venueId"],
        "eventPriceCents": 0,
        "eventCapacity": 1500,
        "eventIsPublic": True,
        "eventType": "concert",
        "eventStatus": "announced",
        "eventCurrency": "USD",
        "eventTicketUrl": "https://www.ticketshow.com.ec",
        "eventArtists": [{"artistId": artist_id, "artistName": "Verde 70"}],
    })
    print(f"   Created Quito event ID: {event_quito['eventId']}")
    
    event_gye = create_event({
        "eventTitle": "Verde 70 - Gira Fulgor (Samborondón)",
        "eventDescription": "Concierto de Verde 70 presentando su nuevo álbum 'Fulgor' en Samborondón. Incluye clásicos como 'Me tienes, me puedes, dueles', 'Palabras' y 'En la inmensidad', junto a nuevas canciones con electro pop, huayno andino, bolero, cumbia y tango.\n\nFecha: 6 de junio de 2026\nHora: 20:00\nLugar: Arena Park, Samborondón\n\nEntradas disponibles en TicketShow.",
        "eventStart": "2026-06-06T20:00:00-05:00",
        "eventEnd": "2026-06-06T23:00:00-05:00",
        "eventVenueId": venue_gye["venueId"],
        "eventPriceCents": 0,
        "eventCapacity": 3000,
        "eventIsPublic": True,
        "eventType": "concert",
        "eventStatus": "announced",
        "eventCurrency": "USD",
        "eventTicketUrl": "https://www.ticketshow.com.ec",
        "eventArtists": [{"artistId": artist_id, "artistName": "Verde 70"}],
    })
    print(f"   Created Samborondón event ID: {event_gye['eventId']}")
    
    print("\n✅ Done!")


if __name__ == "__main__":
    main()

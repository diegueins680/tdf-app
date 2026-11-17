# Event creation entry: home panel, art first, artists by name

Contract: EVT-CREATE-ENTRY-001. This change makes event creation and management
reachable from the signed-in home and shortens creation. It grants no new
organizer, finance, publication or account permissions.

## Home panel

Every confirmed session sees "Tus eventos" at the top of the hub (`/fans`,
`/mi-artista` and the manager view of `/inicio`), with two actions that never
depend on data loading: create an event and manage events. Guests and sessions
still hydrating see nothing.

The panel previews up to three upcoming events whose organizer is the viewer. It
asks the existing event list for upcoming events (largest page, 500) and matches
the organizer on the client, because the list has no organizer filter. An
organizer whose event falls beyond the first 500 upcoming rows does not see it in
the preview; the management page remains complete. The empty state therefore
says "no upcoming events", not "no events". A failed list keeps both actions and
offers a retry.

## Art first

The creator opens on the event art picker, ahead of every other field. The
browser only opens a file dialog on a user gesture, so the page presents the
picker first rather than opening the dialog by itself. The file is validated
before anything is created (JPEG, PNG, WebP or GIF, 1 byte to 10 MB, the server
limits) and is optional. It is held in memory only: a saved draft restores the
other fields, never the file.

The image endpoint needs an event identifier, so the art is uploaded after the
event exists. A refused upload keeps the event and offers a retry with the same
file.

## Artists by name

A lineup entry is an artist profile (`artistId`) or, without one, a name. The
server never drops an entry: without an identifier it needs a name of at most 200
characters without control characters, unique in the request ignoring case and
repeated whitespace.

Inside the event write transaction a name resolves to the single stored profile
with that name; with none, a profile is created without an owning party; with
several, the write is refused with 409 and nothing is written, so the caller
selects the intended artist. A refused lineup creates neither artists nor the
event. The response of a write that named artists is read back and carries the
created identifiers.

A created profile has a name only. Claiming it by the artist is not part of this
change. Two concurrent writes naming the same new artist can create two profiles;
later writes then get the 409 above until one is selected by identifier.

The creator sends known artists with the creation request and named artists in a
following update of the same core fields, omitting image, ticket link and
visibility so the update keeps what is stored. A server that does not accept
names yet refuses only that update: the event exists and the names are offered
for retry.

## Validation

```sh
cd tdf-hq-ui
npm test -- --runTestsByPath src/components/events/MyEventsPanel.test.tsx src/components/events/EventLineupPicker.test.tsx src/pages/CollaborativeEventCreatorPage.test.tsx src/pages/collaborativeEventCreation.test.ts src/pages/FanHubPage.onboarding.test.tsx
npm run typecheck
cd ../tdf-hq
stack test --ta '--match "validateEventArtistRefs" --match "planEventArtists and materializeEventArtists"'
```

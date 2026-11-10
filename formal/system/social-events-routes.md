# `/social-events` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading the handlers on main `6033b84e`
(`tdf-hq/src/TDF/Server/SocialEventsHandlers.hs`, abbreviated **H**, and the
modules it calls). This describes what the code does today. It is **not** an
owner specification: every new requirement written from it carries status
`implemented but unspecified`, and the owner confirms or corrects the intended
behaviour.

Every route below is authenticated (`"social-events" :> SocialEventsAPI` inside
`ProtectedAPI` under `AuthProtect "bearer-token"`, `tdf-hq/src/TDF/API.hs`),
except `POST /social-events/stripe/webhook`, which is public and authenticated by
the Stripe signature.

**Shared guards**
- `requireEventManager` (H:9610): the stored organizer only. Another party gets 403, and an event with no organizer gets 403. Strict administrators are not exempt.
- `requireManagedEvent` adds the event-visibility check.
- `requireRefundManagedEvent` also admits strict administrators.
- `requireLogisticsAccess` / `requireLogisticsOwner`: the owner is the organizer; otherwise the caller's logistics member role decides. There is no administrator bypass.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| ticket-orders (create, refund, status), tickets check-in and transfer, transfer accept, refunds approve/reject, stripe create-payment-intent | EVT-TICKET-001 | reviewed |
| ticket-orders manual-payment review, refunds bank-transfer-complete | EVT-TICKET-MANUAL-001 | reviewed |
| budget-lines update | PAY-EVENT-REPORT-RANGE-001 | reviewed |
| live-broadcasts create/end/heartbeat | MEDIA-STREAM-001 | reviewed |
| memory-invitations (event campaign, open, own preferences) | SOCIAL-EVENT-INVITATION-001 | reviewed |
| moments create, image, media, edit, delete | SOCIAL-EVENT-MEMORY-001 | reviewed |
| events update, delete, image | EVT-OWNER-CLAIM-001 | reviewed |
| events/{eventId}/rsvp PUT | EVT-DISCOVERY-001 | named |
| ticket-tiers, promo-codes | EVT-TIER-PROMO-001 | new |
| waitlist join/remove/notify | EVT-WAITLIST-001 | new |
| ticket-transfers cancel, tax-documents retry | EVT-TICKET-OPS-001 | new |
| budget-lines create, finance-entries | EVT-FINANCE-ENTRY-001 | new |
| logistics (12 routes) | EVT-LOGISTICS-001 | new |
| invitations | EVT-INVITATION-001 | new |
| moment comments and reactions | SOCIAL-EVENT-INTERACTION-001 | new |
| events create | EVT-CREATE-001 | new |
| venues | EVT-VENUE-001 | new |
| artists create/edit | ART-PROFILE-WRITE-001 | new |
| artist follow/unfollow | ART-FOLLOW-001 | new |
| events/{eventId}/rsvp DELETE | EVT-RSVP-WITHDRAW-001 | new |
| me/city-subscriptions | EVT-CITY-SUBSCRIPTION-001 | new |
| event-sources, event-research | EVT-RESEARCH-001 | new |
| stripe/webhook | PAY-STRIPE-WEBHOOK-001 | new (critical) |

The new requirements give each route's authorization, validation, effects and
concurrency handling. Their `ambiguity` fields carry the behaviours that look
unintended.

## Defects fixed on 2026-11-07

1. **Artist profile reassignment.** `PUT /social-events/artists/{id}` applied `artistPartyId` from the request body without checking it. Any owner could therefore move their artist profile onto any other party. Creation already forbids this for non-administrators.
   - Fix: `validateArtistProfilePartyChange`. The owner keeps the stored party, and only a strict administrator may change it.
   - Tested in `tdf-hq/test/Spec.hs`.
2. **Shared city overwrite.** `PUT /social-events/me/city-subscriptions` overwrote the name and timezone of an existing shared city row with the subscriber's input. One user's subscription could therefore change the timezone every other subscriber and event search sees.
   - Fix: existing city rows are reused unchanged.
   - Tested in `tdf-hq/test/TDF/Social/FollowHandlerSpec.hs`.

## Open behaviours for the owner to decide

**Money and records**
- Stripe webhook: when the evidence for a succeeded payment does not match the order, the legacy ticket path answers only 400. It records no reconciliation exception, unlike the canonical capture paths (PAY-CAPTURE-RECON-001).
- Promotion codes are unique across all events, not per event.

**Concurrency and data integrity**
- Waitlist joins have no unique key, so concurrent duplicates are possible.
- Event creation writes the event and its artists in two transactions.
- Several edits have no version check: promotion codes, finance entries, venues, artists and logistics places.

**Who can do what**
- Any signed-in account can create an event.
- Organizers cannot cancel a transfer they did not create.
- Outside canonical interaction mode, comments skip the write budget and the session re-check, and they store a client-supplied author name.

**Missing notifications**
- Waitlist "notify" sends no message.
- Creating an invitation sends no notification.

**Small defects**
- Transfer cancel reads its `{transferCode}` segment as the numeric transfer id.
- Venue creation is not synced to directory search until the venue is edited.

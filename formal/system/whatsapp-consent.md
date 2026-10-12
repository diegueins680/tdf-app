# WhatsApp consent double opt-in (PRIV-WHATSAPP-001)

Reconstructed from `tdf-hq/src/TDF/Server.hs` (`publicRequestWhatsAppConsent`,
`claimWhatsAppConfirmationRequest`, `releaseWhatsAppConfirmationClaim`,
`applyWhatsAppConsentConfirmation`, `publicWhatsAppOptOut`,
`whatsappConsentRoutes`), `tdf-hq/src/TDF/CampaignAutomation.hs`
(`applyWhatsAppCampaignOptOut`, `activeConsent`) and the migration
`tdf-hq/sql/2026-10-07_whatsapp_consent_confirmation.sql` on 2026-10-12.
The requirement text was read against the code, not the other way round.

## State

One `whats_app_consent` row per E.164 number (`UniqueWhatsAppConsent`).
Both the public form (`normalizeInternationalPhoneInput`) and the inbound
webhook (`normalizeWhatsAppPhone` on the message's `from`) reduce a number to
`+digits`, so a reply is matched to the row of its own number.

| Column | Meaning |
| --- | --- |
| `consent`, `consented_at` | active consent (campaigns also require `revoked_at IS NULL`) |
| `confirmation_requested_at` | the pending request, or NULL |
| `revoked_at` | the last withdrawal |
| `note` | `pending_confirmation`, `confirmed_by_reply`, `consent` (admin), opt-out notes |

## Transitions

1. **Claim** (public request). The row is created if absent, then one
   conditional update sets `confirmation_requested_at = now` where
   `consent = false` and the previous request is NULL or older than 24 hours
   (`whatsAppConfirmationResendInterval`, strict). Only a claimed request
   sends the confirmation message (template `WHATSAPP_CONSENT_TEMPLATE` or
   free text). A refused send releases the claim: one update that clears the
   request only where it still equals the claimed time and `consent = false`.
   The public response is the same whether or not a claim was made.
2. **Confirm** (inbound affirmative keyword `si`, `sí`, `yes`, `acepto`,
   `confirmo`, exact after trimming). One conditional update sets consent
   where `consent = false`, `confirmation_requested_at >= now - 7 days`
   (`whatsAppConfirmationWindow`) and `confirmation_requested_at <= sentAt`,
   the message's own Meta timestamp. It clears `revoked_at` and the request.
   A message without a timestamp confirms nothing.
3. **Withdraw** (public opt-out form, inbound `stop`/`salir`/`cancelar`/`baja`,
   admin revoke). One upsert sets `consent = false`, `revoked_at = now` and
   clears the request. The public opt-out messages the number only if it was
   subscribed.
4. **Admin capture**. The gated route upserts `consent = true` directly.

Campaign enrollment reads `activeConsent`: `consent`, a `consented_at` and
no `revoked_at`.

## Concurrency

Every transition after creation is a single statement on the unique row, so
PostgreSQL serializes them on the row lock and re-evaluates each predicate
after the earlier writer commits: a withdrawal committed before a
confirmation empties the confirmation's predicate (request cleared); a
confirmation committed before a withdrawal is overwritten by it; a new claim
after the interval and a reply to the old request resolve to exactly one of
the two (`scripts/__tests__/whatsapp-consent-postgres.mjs`, both orders).

Row creation was not race-safe. `claimWhatsAppConfirmationRequest` used
`insertUnique`, which persistent implements as a lookup followed by a plain
insert. Concurrent first requests for one number therefore all passed the
lookup, and every insert but the first raised
`duplicate key value violates unique constraint "unique_whats_app_consent"`;
the handler answered HTTP 500 to those submitters. Reproduced on 2026-10-12
against the real backend (`tdf-hq-exe` on main `687d7d401`) with four
simultaneous public requests: one 200, three 500, one pending row. Consent
state was never wrong (one claim, one message), so this is an availability
defect in the requirement's "one conditional update" claim path, fixed in
this slice by creating the row with `INSERT ... ON CONFLICT (phone_e164) DO
NOTHING` before the conditional claim. The PostgreSQL test keeps the
check-then-insert shape as a negative control.

## Model

`formal/event-operations/WhatsAppConsent.tla` has one number, a server clock
(0..6), a resend interval of 2 and a window of 4 ticks, and up to two
opt-outs (epochs). Actions: `Claim`, `SendAccepted`, `SendRefused`
(release), `Reply(sentAt)` for every `sentAt` up to the clock (delivery of a
message sent at any earlier time, repeatable), `OptOut`, `AdminCapture`,
`Tick`. The claim remembers its epoch. Invariants:

- `ConsentFromTimelyReply`: consent by reply rests on a request made no later
  than the reply was sent and confirmed inside the window.
- `WithdrawalWins`: the confirmed request belongs to the current epoch, so no
  withdrawal happened after the request that the active consent rests on.
- `OneMessagePerInterval`: two accepted confirmation messages in one epoch
  are more than the interval apart (a withdrawal legitimately allows an
  immediate new request).
- `ConsentEvidenced`: consent never exists without evidence.

Positive run: 183,500 states, 48,824 distinct, depth 20, exit 0. Controls,
each removing one guard, fail on the named invariant: `StaleReply`
(`ReplyAfterRequest = FALSE`, a reply sent before the request confirms it),
`Expired` (`WindowEnforced = FALSE`), `OptOutKeepsRequest`
(`ClearOnOptOut = FALSE`, a later reply confirms a request made before the
withdrawal), `ReadThenWrite` (`ConditionalConfirm = FALSE`, the confirmation
decides and writes in two steps and a withdrawal lands between them),
`NoInterval` (`IntervalEnforced = FALSE`).

The creation race is below the model (an atomic `Claim`); the PostgreSQL
test covers it. Bounded model checking; not a proof of the Haskell or SQL.

## Evidence

- `scripts/__tests__/whatsapp-consent-postgres.mjs` (CI step "Verify
  WhatsApp consent claim, confirmation and withdrawal under concurrency"):
  mirrored statements, single-session boundaries (interval strictness,
  release of the own claim only, reply at the exact request time, window
  edge, redelivery, no claim on active consent), and the three two-session
  races above with the pre-fix creation shape as a negative control.
- `scripts/lib/whatsapp_consent_conformance.py` in the booking conformance
  harness: four simultaneous public requests against the real backend (all
  200, one pending row, one attempted message), signed Meta webhook replies
  with their own timestamps (stale, non-affirmative, confirming, redelivered,
  STOP, late reply after withdrawal, new request after withdrawal, expired
  request, unknown number). The fixture backend has no provider, so claims
  are released and the pending window is prepared in the row.
- `tdf-hq/test/TDF/ServerSpec.hs`: keyword, window and claim-interval unit
  tests (SQLite).

## Ambiguities recorded

- The window is measured from receipt (`now - 7 days <= request`), not from
  the reply's `sentAt`; a reply sent inside the window but delivered after it
  does not confirm. Stricter than the requirement's wording; kept.
- A reply's `sentAt` may lie in the future relative to the server clock (clock
  skew); only its lower bound matters.
- A released claim leaves `revoked_at` and `note` as they were; `note` stays
  `pending_confirmation` after a refused send.
- Two public requests inside the interval both answer 200; only the first
  sends. The response discloses nothing either way.

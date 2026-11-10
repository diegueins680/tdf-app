# Social, messaging and webhook mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Social/*`, `Interactions/Server.hs`, `Server/Reviews.hs`, `Reputation.hs`, `Server/SocialDiscovery.hs`, `Server/SocialSync.hs`, `ServerLiveSessions.hs` and the WhatsApp, Instagram and Facebook sections of `Server.hs` and `ServerExtra.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- The 24 non-webhook routes are behind `AuthProtect`; social, chat and interaction writes re-validate the session under row locks. Staff outbound messaging needs `hasSocialInboxAccess`; WhatsApp consent capture, opt-out and Meta backfill need the plain Admin role; discovery review and social sync need a strict administrator.
- The four webhook aliases verify the Meta `X-Hub-Signature-256` over the raw body before any effect (AUTH-WEBHOOK-001) and store inbound rows once.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| friends add/remove, vcard, v2 relationships and preferences | SOCIAL-RELATIONSHIP-WRITES-001 | new |
| discovery review, chatkit, social-sync ingest, meta backfill | SOCIAL-STAFF-TOOLS-001 | new |
| chat dm, messages | MSG-CHAT-WRITES-001 | new |
| interactions commands, blocks, preferences | SOC-INTERACTION-WRITES-001 | new |
| reputation consents, preferences | REP-SELF-SERVICE-001 | new |
| navigation preference, visit | NAV-PREFERENCES-001 | new |
| whatsapp reply, operator-question, consent, opt-out; instagram and facebook reply | MSG-STAFF-OUTBOUND-001 | new (critical) |
| live-sessions intake | LIVE-SESSION-INTAKE-001 | new |
| hooks/whatsapp, webhooks/whatsapp, instagram/webhook, facebook/webhook | AUTH-WEBHOOK-001 (with MSG-*-REDELIVERY-001) | reviewed |

MSG-ACCESS/ATOMIC/NOTIFY-001 (chat), SOC-ABUSE-001 (write budget), PRIV-WHATSAPP-001 (consent) and AUTH-SOCIAL-OAUTH-001 (backfill sources) hold on these routes.

## Open behaviours for the owner to decide

**Consent (critical)**
- `POST /whatsapp/reply` never consults `whats_app_consent`: staff can message a number that sent STOP. It also does not mark the inbound message replied.
- Staff consent capture sets consent immediately with no double opt-in; opt-out confirms even for a never-subscribed number.

**Authorization**
- Consent, opt-out and backfill are gated by the plain Admin role while their siblings require a strict administrator; the social v2 preferences route skips the process-level feature gate.
- Any account can create party rows through live-session intake without a rate limit.

**Concurrency**
- Discovery reviews, social-sync ingest and the legacy chat thread open race into unique constraints (500); chat sends and Meta replies have no send-once guard.
- The social-sync ingest path carries a duplicated `social-sync` segment.

# Persona experience and heuristic quality assessment

These are **simulated persona-based hypotheses**, not reports from people. Zero participants were recruited. Completion times, assistance counts and sentiment are not invented. Only the five partial story slices listed in [execution results](execution-results.json) have automated or direct-observation evidence.

## Transparent rubric

Each future exploratory run records observable facts on five 0–2 dimensions: discoverability, clarity/feedback, recovery, accessibility/responsiveness and trust/price/privacy clarity. `0` means the acceptance criterion failed, `1` means completion required a workaround or ambiguity remained, and `2` means independently completable with clear state. The ten-point sum is a local heuristic, not SUS, UMUX-Lite, NPS or a participant score. This report does not assign scores before the journeys are run.

## Per-persona hypotheses and evidence needs

| Persona | Heuristic experience hypothesis | Evidence required before treating it as fact |
|---|---|---|
| PER-01 Elena, visitor | Public discovery should survive slow connectivity and preserve intent at login; pricing/auth prompts may still be a trust threshold. | Partially automated for search/auth redirect/reflow; real registration, password recovery and ticket offer remain unexecuted. |
| PER-02 Mateo, fan/customer | Ticket cancellation and ambiguous payment guidance reduce duplicate-charge anxiety; cross-device issuance remains the key trust moment. | Mobile component/idempotency passed; PostgreSQL seed/login/session/logout and stale-cookie denial passed after PT-011; profile UI, sandbox provider, delivery and validator lifecycle required. |
| PER-03 Claire, international customer | Mixed locales, exchange-rate explanation and non-color refund status may block confidence for international payment. | English locale audit, currency/provider contract and refund timeline not executed. |
| PER-04 Valeria, artist/fan/customer | Multi-epic handoffs from private profile draft to studio booking and release submission could expose inconsistent ownership terminology. | Disposable backend/database plus fake distribution partner required. |
| PER-05 Bruno, artist/producer/customer | Composed roles should feel additive without forcing an unexplained mode switch; tablet keyboard and cross-device continuity are risks. | Role-composition integration and tablet exploratory navigation required. |
| PER-06 Nicolás, collaborator | Search synonyms and response privacy will determine whether classifieds feel useful rather than spam-prone. | Search-index fixture, moderation/block/report and duplicate-post tests required. |
| PER-07 Camila, producer/A&R | Lead-to-session-to-release provenance should stay visible; overly broad CRM/release access would damage trust. | Assigned-scope negative API tests and full pipeline history required. |
| PER-08 Sofía, A&R/LabelRep | Keyboard-complete metadata review is valuable, but incomplete DDEX delivery prevents goal completion. | DDEX is a documented gate; keyboard and contract-scope tests remain. |
| PER-09 Andrés, teacher/artist | Schedule conflict prevention and audited attendance edits should matter more than dense administration. | Course/trial/attendance database lifecycle and tablet touch inspection required. |
| PER-10 Lucía, fictional minor | Guardian consent and shared-device logout must be explicit; commercial consent cannot be inferred from the minor. | Legal/product consent decision, guardian fixture and isolation tests required; no real minor testing without safeguards. |
| PER-11 Martina, intern | Concealed privileged actions and clear supervisor boundaries should reduce accidental harm; evidence resubmission must be safe. | Existing role tests should be extended with direct URL/API and assigned-record cases. |
| PER-12 Karla, reception | Duplicate-resistant lead capture and booking conflict feedback should reduce call handling friction; finance controls must stay absent. | Lead-to-reconciliation integration with two actors and local notifications required. |
| PER-13 Fernando, studio manager | A consolidated conflict/assignment/revenue view could improve decisions, but stale state can make confident actions unsafe. | Concurrent booking/assignment and reconciled report lifecycle required. |
| PER-14 Rosa, maintenance | Offline drafts and unmistakable unavailable status should prevent unsafe equipment booking; color alone is insufficient. | Detox/offline sync plus backend availability conflict tests required. |
| PER-15 Diego, webmaster | Bilingual preview/rollback and diagnostics redaction should support safe publishing without granting user/finance administration. | CMS version/audit tests, secret-shape scan and public cache verification required. |
| PER-16 Alma, administrator | Exact role changes, forced session refresh and auditable dual-control boundaries are essential; broad admin labels may conceal composition rules. | Backend role lifecycle tests and screen-reader audit UI inspection required. |
| PER-17 Javier, venue representative | Claim/review status and private-vs-public field boundaries should be easy to understand; venue detail now has a native mobile route. | Claim ownership API tests plus native/web projection parity required. |
| PER-18 Natalia, marketplace buyer | Total cost, dated availability, deposit authorization/capture/release and dispute evidence drive purchase trust. | Multi-party rental, fake provider and refund/reconciliation tests required. |
| PER-19 Óscar, seller/owner | Immutable handoff evidence and understandable settlement/cancellation rules should prevent disputes and support revenue. | Buyer/seller/finance lifecycle and custody append-only checks required. |
| PER-20 Renata, organizer | Capacity, QR replay denial, cancellation/refund and payout liability must remain coherent through event closeout. | Ticket backend integration and offline validator behavior required. |
| PER-21 Samuel, distribution client | Actionable metadata rejection and idempotent redelivery would build trust; incomplete partner delivery currently blocks completion. | Fake partner adapter, private storage, acknowledgement and rights-scope tests required. |
| PER-22 Paula, accounting | Evidence-first reconciliation and dual control should be efficient without exposing customer administration; exports introduce formula/redaction risk. | Two-actor security integration and sanitized CSV inspection required. |
| PER-23 Iván, Live Sessions producer | Consent, room/equipment assignments and collaborator revocation should read as one production lifecycle rather than disconnected modules. | Cross-module backend/database journey and fake notifications required. |
| PER-24 Marisol, screen-reader customer | A single main landmark, clear errors, keyboard operation and reflow are prerequisites to independent booking. | Public auth/search landmark/axe slice passed; manual screen-reader and checkout/course journeys remain. |
| PER-25 Luis, road crew | Large touch targets, offline custody and minimum attendee data should support event-day work in poor connectivity. | iOS Detox and an Android configuration/physical-device pass required. |
| PER-26 Daniela, DJ/Domo customer | Mobile web fallbacks must retain filters and checkout state; Domo inquiry-to-deposit clarity determines conversion. | 2G throttling, cross-device state, Domo quote/payment/reconciliation lifecycle required. |

## Accessibility findings

- Automated: the covered login and directory states have zero axe violations classified serious/critical after PT-003; 320 CSS-pixel reflow has no horizontal document overflow; the covered shell exposes exactly one `main#main-content` after PT-009.
- Source/component: mobile ticket tests cover accessible quantity controls, unavailable tier state, inline validation, pending/cancelled guidance and non-PII idempotency storage keys.
- Not demonstrated: full keyboard traversal, visible focus, dialog return focus, screen-reader announcements, 200% text-only zoom, forced-colors/high-contrast mode, color-vision simulation, reduced-motion animation inventory, target size and native accessibility-tree semantics across all journeys.
- Manual gate: VoiceOver/TalkBack/NVDA or equivalent testing with representative users remains necessary; axe cannot establish screen-reader usability.

## Localization and consistency findings

Spanish remains the default and the synthetic catalog covers `es-EC`, `es-CO`, `es-MX`, `es-CL`, `es-AR`, `en-US` and `en-CA` contexts. The executed browser suite used `es-EC` only. English completeness, regional dates/times, currency/tax language, pluralization, untranslated provider errors and cross-border refund terminology remain untested. A locale fallback now survives malformed catalogs, but that resilience check is not a translation-quality claim.

## UX observations from executed slices

- Direct observation: the directory search preserves query and city state through detail/back navigation and renders at phone/tablet/desktop sizes.
- Direct observation: a rejected login exposes a visible alert and retains the protected redirect target.
- Direct observation: malformed optional catalog data used to blank the app; the recovery fix now leaves the shell usable.
- Direct observation: PER-02's deterministic Fan + Customer account authenticated against local PostgreSQL and received only its expected roles/module; pre-fix logout left a replayable active token, while post-fix stale-cookie replay returned `null` and the database reported zero active persona tokens.
- Heuristic hypothesis: the login flow will feel safer when password recovery and registration are visible from a preserved intent, but the full flow was not executed.
- Heuristic hypothesis: financial trust depends on authoritative pending/cancelled/reconciled wording and totals, which cannot be assessed from provider-mocked components alone.

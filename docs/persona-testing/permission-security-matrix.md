# Permission and security matrix

The backend—not route visibility—is the authority. The generated 3,058-row role–module–feature–action–platform matrix is the exhaustive surface artifact; this document defines persona-focused positive and negative checks. `Specified` means the test is designed but was not executed against a running backend. The Haskell backend and its final 2,420-example suite compile and pass locally, including the seed guards and logout revocation regression; a focused session run also passed 2/2. A running disposable PostgreSQL service verified seed role composition and the PER-02 login/logout slice, but the complete direct-API matrix was not executed.

| Persona / role combination | Authorized capability to prove | Direct URL/API denial to prove | Current evidence |
|---|---|---|---|
| PER-01 Visitor | Public published projections only | Private records, mutation and guessed tracker capability denied | Auth redirect/browser slice passed; API specified |
| PER-02 Fan + Customer | Own profile, preferences, orders and tickets | Other customer order/ticket; admin/staff modules | PostgreSQL seed/login/session/logout roles and token revocation passed; profile/ownership breadth specified |
| PER-03 Fan + Customer | Own international order/refund status | Raw provider evidence, another buyer record | Specified |
| PER-04 Artist + Fan + Customer | Own artist/profile/release drafts and bookings | Other artist/release; label-wide approval/delivery | Specified |
| PER-05 Artist + Producer + Customer | Explicit union of owned artist/producer/customer actions | Private collaborator projects not assigned to actor | Specified |
| PER-06 Artist + Songwriter + Customer | Own classified/applications and public discovery | Moderation and other author's edits | Specified |
| PER-07 Producer + AandR + Customer | Assigned lead/session/release scope | Unassigned CRM leads and label catalog | Specified |
| PER-08 AandR + LabelRep | Assigned catalog, DDEX validation and delivery status | Finance admin and unrelated label contracts/assets | Specified; DDEX gated |
| PER-09 Teacher + Artist | Own teaching schedule and assigned students | Unassigned student attendance/guardian data | Specified |
| PER-10 Student + Customer | Own approved learning records | Guardian consent mutation and commercial consent bypass | Specified; product/legal gate |
| PER-11 Intern | Assigned internship tasks/evidence | Admin, finance, role changes and unassigned operations | Specified |
| PER-12 Reception | CRM lead capture and scheduling | Payment reconciliation/refund/admin | Specified |
| PER-13 StudioManager | Studio rooms, assignments, conflicts and scoped reports | Strict role/security administration unless separately granted | Specified |
| PER-14 Maintenance | Assigned equipment condition/maintenance state | Customer order ownership, financial state and unauthorized restore | Specified |
| PER-15 Webmaster | CMS, public content and safe diagnostics | Users/roles, finance and secret-bearing provider configuration | Specified |
| PER-16 Admin | Exact configured administrative capabilities, audited grants/revocations | Self-review/self-approval and incoherent strict-admin shortcuts | Existing role tests plus specified backend lifecycle |
| PER-17 Venue representative | Claimed/verified venue record and owned inquiries | Pre-approval edits, other venue/private claimant data | Specified |
| PER-18 Customer/buyer | Own cart/rental/deposit/refund status | Seller controls, provider evidence and other buyer orders | Specified |
| PER-19 Vendor + Customer | Owned listings, requests, custody and settlement view | Other seller listing/custody edits; finance reconciliation | Specified |
| PER-20 Promoter + Customer | Owned event, tiers, validator and summary | Other organizer events; finance-wide payout controls | Specified |
| PER-21 LabelRep + Artist + Customer | Owned label/release/import/delivery/order | Other label assets/partner credentials/admin | Specified; delivery gated |
| PER-22 Accounting | Reconciliation, refund preparation and sanitized exports | User administration; self-approval if dual control applies | Specified |
| PER-23 LiveSessionsProducer + Producer | Assigned production/session/resources | Unassigned consent/media and finance controls | Specified |
| PER-24 Customer + Fan | Own accessible booking/course/account records | Staff notes, other student/customer records | Public browser slice only |
| PER-25 RoadCrew + StageManager | Assigned event-day operations/custody | Attendee PII and unassigned events/equipment | Specified |
| PER-26 DJ + Customer | Own Domo/marketplace/checkout state | Staff quote approval, reconciliation and other inquiries | Specified |

## Required negative-test pattern

For every non-public story, fixtures must execute the same attempted action through four paths: normal UI discovery, direct protected URL, API client, and raw request with a valid token belonging to the wrong persona. Assert the status (`401` for unauthenticated, `403` for known-but-unauthorized where disclosure is safe, or ownership-safe `404` where concealment is required), no database/outbox/audit side effect except an authorized security-denial event, and no existence leak in timing or message.

Object-isolation tests pair owners and non-owners: buyer/seller, artist/collaborator, student/teacher/guardian, customer/reception, organizer/attendee/validator, label client/distribution admin and requester/system admin. Guessed identifiers, stale role tokens and revoked sessions must not bypass record scope.

## Multi-role and role-change rules

- Roles compose only through explicitly registered role/module/feature/action grants. UI label composition is not permission evidence.
- Strict-administrator shortcuts must satisfy the repository's coherent-admin rule; a role name alone must not grant unsupported actions.
- Adding one role should make its exact actions visible after a predictable session refresh; revocation must invalidate or refresh stale sessions and shortcuts.
- A role change never changes ownership, self-review restrictions, dual control or consent obligations.
- Unsupported actions remain unsupported; they do not inherit `view` or another broad capability.

## Non-destructive security checklist and status

| Control | Safe check | Status |
|---|---|---|
| Authentication/session | Rejected login, redirect preservation, logout/back history, expiry and reset-token replay | Rejected login and successful PER-02 login passed; PT-011 fixed and stale-cookie replay denied; expiry/reset/browser-history breadth specified |
| CSRF/CORS | Same-origin cookie mutation and approved-origin preflight; no hostile traffic | Not executed; local backend existed but no browser-origin/preflight harness was run |
| Ownership/IDOR | Wrong-persona token against every owned record | Specified |
| Rate limits | Small bounded burst against registration/login/public inquiry in disposable environment | Not executed; no high-volume testing authorized |
| Uploads | Fake file type/size/path, private URL and malware-adapter contract | Specified; DDEX storage absent |
| Sensitive output | Search UI/API/log/export artifacts for token/card/secret/PII shapes | Browser capture sanitizes; full backend/export scan specified |
| OAuth | State/nonce/redirect/expiry tests with local fake; no live account | Existing units passed within web baseline; end-to-end not executed |
| Provider callbacks | Invalid signature, amount/currency/order mismatch, duplicates and out-of-order events | Existing source/tests inventoried; integrated run blocked |
| Idempotency | Same key/same payload, same key/different payload, lost response and closed checkout | Mobile ticket component/storage slice passed; backend specified |
| Privacy/consent | Opt-in notifications, minor guardian gate, public projection minimization and deletion/retention | Specified; requires product/legal decisions for minors |

No intrusive penetration test, denial-of-service test, CAPTCHA trigger, account lock, real upload, real OAuth, real payment or production authorization mutation was attempted.

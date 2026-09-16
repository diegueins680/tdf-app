# Canonical graph and enforcement policy

## Identity and ownership

`ApiToken` authenticates a principal acting as a `Party`. A Party can represent a
person or organization; it is not itself proof of login eligibility. Existing
`ArtistProfile`, `SocialEvents.ArtistProfile`, `Band`, `Venue` and `FanClub` remain
authoritative for their resources. Do not collapse social artist IDs into PartyIds.
Band membership, catalog grants and resource ownership remain separate authorities.
A managed entity cannot be used to bypass a principal block; support for this
requires retaining both principal and acting entity at the authorization boundary.
The account-only pilot now retains and revalidates API token identity at the
[session boundary](session-boundary.md). Separate credential/session provenance,
delegated entity authority and global-role revocation remain cutover work; never
infer missing authority from graph proximity.

| Edge | Direction/cardinality | Owner and lifecycle | Rights |
|---|---|---|---|
| follows | directed unique source/target, no self; reciprocal allowed | source; active/removed, immutable creation time, versioned removal | subscription only |
| connection consent | two independent intents per unordered pair, no self | each principal owns its intent; pending -> both accepted -> disconnected; block clears both | communication only when both valid |
| block | directed unique, reciprocal allowed | blocking principal; block/unblock never restores consent | deny interactions in either direction |
| mute/dismiss | directed unique | viewer; reversible private preference | presentation exclusion, never changes another user's rights |
| membership | existing entity/member/role constraints | owning domain; grant/change/revoke | only explicitly defined scoped authority |
| content/authorship/replies | source post, author, parent/club | fan-club domain; hide/edit/delete | no graph-derived edit permission |
| RSVP/ticket/booking/purchase | existing domain keys and consent | event/commerce authority | entitlement never endorsement or general membership |

All derived feed/search/recommendation entries carry references, not independent
rights. Rebuild from authoritative live state. Deleted identities/relationships
must retain revision/tombstone information for the supported retry window; stale
commands with an earlier revision must conflict. No automatic consent backfill:
legacy reciprocal PartyFollow edges can have been manufactured by a single actor.

## Policy precedence

| Actor/resource/action | Allow conditions | Denial precedence / field rules |
|---|---|---|
| Anonymous protected read or mutation | none | authenticate first; generic denied result |
| Self preferences | authenticated principal owns record and is active | no impersonated party input |
| Follow | active source/target, no block, source intent | no access to private fields merely from follow |
| Connect/accept | each party acts for itself; accept requires other intent | block/deletion/suspension beats existing consent |
| Disconnect/block | active owner, target relation valid | revocation serialized with acceptance/send; unblock needs new consent |
| Protected club content | existing club officer/fan authority and live visibility | hidden/deleted excluded; social connection cannot substitute membership |
| DM send | thread participant + bilateral explicit consent + no block | administrator has no ordinary social block bypass |
| Recommendation/reason/count | eligible public candidate; consented signals | filter before scoring/aggregation/limit; no confidential transactions or mutual names |
| Cached/projection read | current authority independently allows it | stale cache can deny availability, never grant access |
| Queue delivery/retry | current authority at transaction boundary; matching revision and dedup key | revoked/deleted/stale work terminates without delivery |
| Moderation | explicit existing scoped moderation permission | auditable reason/action; retain existing review/appeal flow |

A read linearizes at its authoritative database snapshot. A revoke committed
before that snapshot must deny it; an overlapping read may complete from the prior
snapshot. A write/delivery linearizes under the same ordered entity/pair locks as
revocation. Network delivery already accepted by an external service cannot be
recalled. Public internet copies and recipient devices are outside this boundary.

```mermaid
sequenceDiagram
  participant A as Acceptance/send
  participant DB as PostgreSQL authority
  participant B as Block/revoke
  A->>DB: Lock actors/pair in canonical order
  B->>DB: Wait for same locks
  A->>DB: Revalidate, mutate + record command, commit
  B->>DB: Revoke, advance version, commit
  A->>DB: Retry old command
  DB-->>A: Current denial/conflict; no restored consent
```

Do not activate new privacy controls until APIs, profile selectors, media previews,
counts, old clients, shared-space mentions and notification delivery all enforce the
same policy. Private preference logs must contain codes/aggregate counts, not
message text, contacts, exact location or relationship lists.

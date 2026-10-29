# Artist catalog and audience consolidation

## Implemented behavior

Artist profiles, the Fan Hub and Label releases now share the `artistCatalog`
read boundary. The adapter gathers public canonical releases and legacy artist
release references, preserving each source's real identity. It reads canonical
pages of 100, stops on a short page and rejects repeated pagination or a catalog
larger than the explicit 10,000-row bound rather than silently truncating it.
There are at most four simultaneous artist reads (each has two initial source
requests). Cancellation is forwarded to both endpoints. Responses attributed to
another artist are rejected. A failed source or artist remains an explicit
incomplete result while other available entries remain visible.

Canonical published-release cards have one implementation across the music
catalog, public profiles, Fan Hub and Label. Label search/date/artist filters
include canonical publications as well as legacy references. Committed release
changes invalidate all catalog projections that include that artist.

A shared Party identity parser and public-profile route builder now serve event,
community, hub and public-profile links. IDs must be positive, exact JavaScript
safe integers; numeric strings with signs, leading zeroes, exponent syntax or
rounding are rejected. Social artist record IDs cannot substitute for linked
Party IDs. Canonical slugs are used only when valid; otherwise the Party route
is used. No names, emails or matching titles establish identity.

Artist subscription writes from the profile and hub use the same existing
server command boundary. Audience invalidation refreshes the viewer's own
subscription list, the target artist's fan pages and cached public profile aliases,
without invalidating another viewer's private list. Ambiguous writes retain the
existing read/reconcile behavior; no automatic write replay or social consent
is introduced.

## Authority and migration boundaries

The repository inventory at `docs/music-releases/live-catalog-inventory-2026-10-26.md`
records 65 legacy release rows and zero canonical releases at its observation
time. These historical counts are not a new production observation. Legacy
references remain references: this change does not copy masters, infer rights,
create canonical publications or grant stream/download access. Exact duplicate
IDs within a source can collapse; two same-title releases across sources remain
separate until an explicit provenance mapping is established.

Physical catalog migration still requires an owner-authorized source inventory,
source-to-recording mappings and rights/publication review. The client adapter is
not a database migration or the completion of universal audio ingestion.
`social_artist_profile` and `artist_profile` also remain separate physical
relations; linking profile navigation does not complete their data migration.

`FanFollow`, `PartyFollow` and Social V2 cannot be blindly combined. In the active
code, FanEffects grants artist subscriptions and governs historical club side
effects; RelationshipWrites governs connection compatibility and retirement;
Social V2 owns revisioned follow/connection/block/mute state. Subscription,
connection consent and club membership are distinct. This increment consolidates
subscription commands/cache projections and does not backfill consent, reopen
retired writers or activate runtime flags.

The other proposed consolidations are tracked as follows:

| Candidate | Existing authority / next boundary |
| --- | --- |
| Common person/profile fields | `Party` already owns artist display identity; fan and social extensions have role-specific fields. Physical normalization needs field-level ownership and conflict evidence. |
| Genre strings and canonical memberships | Canonical genre references and membership tables already exist; legacy text remains a compatibility/import boundary. Do not guess a genre UUID from an ambiguous label. |
| Product checkouts | `Commerce.CheckoutStore`, `PaymentIntentStore` and provider event/runtime stores already provide common infrastructure. Product adapters preserve inventory, delivery and refund rules; they should not be collapsed into one undifferentiated order. |

No tables, original files, relationship rows or historical links are deleted or
rewritten. No production inventory, migration, flag activation or deployment has
been performed by this source change. Existing player PR #670 remains separate;
its pending media-health, featured-track and native-browser obligations are not
closed by this catalog work.

## Verification

Tests exercise mixed catalogs and stable source identities, pagination and
negative controls for repeated pages/cross-artist data, partial outages,
cancellation, bounded concurrency, exact Party parsing, cache/account scoping,
and Label rendering of both release sources. Existing profile follow/session,
Fan Hub, public catalog, event and Social V2 component tests are retained.
These executable checks do not prove production data correspondence or rights.

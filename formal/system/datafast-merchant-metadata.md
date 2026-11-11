# Dataweb merchant metadata admission: PAY-DATAFAST-001

Datafast API credentials alone do not make a new checkout ready. Every Dataweb
checkout creation must validate the configured MID, TID, PSERV and USER_DATA2
before allocating a provider attempt or calling the gateway. Shared service,
booking, course, ticket, Domo and music creation paths and marketplace creation
must send the same validated merchant parameters. Existing contract, environment,
merchant-account, capability, ownership and replay checks remain mandatory.

The merchant reference fields are nonempty, bounded to 256 characters and use
ASCII provider-reference characters. USER_DATA2 is nonempty text bounded to 30
characters with no control characters. Validation errors contain field names,
never configured values. VERSIONDF retains the documented default 2; an explicit
version is one to four ASCII digits with no leading zero. SHOPPER_ECI is the fixed
identification value 0103910 in the provider's published Dataweb guide. These are
protocol parameters, not proof of successful 3DS authentication.

The shared HTTP form encoder operates on UTF-8 bytes. Names containing accented
characters and values containing ampersands, equals signs or plus signs must
survive form decoding unchanged. No buyer or merchant value may create another
request parameter.

Ticket payment creation must retain the optional buyer_phone saved in the order
metadata. The stored projection uses the same trim, 24-character limit and
character validation as initial ticket input. Missing, null, malformed or invalid
stored phones remain absent; another metadata field never supplies a fallback.
No phone is invented and no new collection or availability requirement is added.

The marketplace and shared service Datafast builders project the same supplied
free-text buyer name. Empty input stays empty; a single name does not become a
fabricated surname, and multiple components retain their existing first/rest
projection. Splitting and rejoining must preserve the original name tokens,
apart from whitespace normalization. This removes the marketplace's former
Cliente/TDF fallback and duplicated mononym. It does not validate legal identity,
decide provider acceptance of mononyms, impose a new naming rule, or complete the
pending structured buyer profile. Existing status/replay/attempt behavior is
unchanged; provider validation and certification remain outstanding.

Creation metadata is not required to query an existing payment. A removed MID,
TID, PSERV or USER_DATA2 must hide fresh checkout availability while preserving
the existing credential-based status query configuration. This does not permit
a new attempt, fallback, capture or paid state without the original verification
and immutable payment binding.

## Immutable cart projection

New Dataweb requests in every existing checkout domain read the ordered line
items from the canonical checkout, not the current catalogue or client-supplied
prices. The stored parent total and currency must match the amount being sent.
The shared projection sends each description, integer quantity and exact USD
unit price as `cart.items[n].name`, `.description`, `.quantity` and `.price`.
It retains line order and never divides a gross total into a rounded invented
unit price. Exact integer products and their sum must conserve the charged total;
empty lines, nonpositive amounts, overflow and unsupported currency are refused
before a new gateway request.

The documented name field excludes ampersands and is bounded to 255 characters;
control characters and blank descriptions are refused without echoing their
contents. Quantity is limited to the documented five-digit integer field and
money to ten whole decimal digits plus two cents. UTF-8 and literal plus/equals
characters survive form encoding without adding parameters. Only the Dataweb
projection is changed; merchant product names are never rewritten.

This first projection accepts canonical parents and lines whose stored subtotal
equals their total and whose adjustment columns are zero. Nonzero discount, tax or fee
adjustments require their own qualified economic projection and are refused;
they are never absorbed into a fictitious item. Zero adjustment columns in this
generic checkout schema do not establish a zero VAT rate. The renderer does not
emit `SHOPPER_VAL_BASE0`, `SHOPPER_VAL_BASEIMP` or `SHOPPER_VAL_IVA`, because the
authoritative fiscal classification is still pending. This cart work does not
establish a complete certifiable Dataweb payload.

Persisted provider checkout reuse and existing payment status verification do
not read or require this new cart projection. Removing or changing an editable
catalogue product cannot change the saved cart. No new API, schema migration,
card-storage capability, merchant activation or retry authority is introduced.

Evidence consists of 21 local Hspec parameter and form-decoding cases, two
100-case properties and six behavior-changing source controls. Eight isolated
PostgreSQL cases exercise the actual canonical cart reader against the four
unchanged checkout table definitions; this is not a complete migration replay.
The existing real SQLite-backed public availability handler tests remove each
required metadata field and retain a usable status configuration. There is no provider sandbox,
3DS, browser-widget, certification, production-charge or all-domain refinement
claim. Provider outcome, concurrency and idempotency retain their existing
contracts; no new lifecycle model or activation authority is introduced.

The provider's public integration guide is
https://developers.datafast.com.ec/Index.aspx (metadata read 2026-10-29; cart
fields read 2026-11-10). TDF's private
onboarding email and attachments remain outside Git. Merchant certification is
still blocked on onboarding, issued test credentials, applicable buyer/cart/tax
fields and actual credentialed qualification. Do not infer tax allocation from a
gross total or replace missing buyer data with fictitious defaults.

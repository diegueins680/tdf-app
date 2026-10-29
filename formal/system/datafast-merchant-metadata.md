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

Creation metadata is not required to query an existing payment. A removed MID,
TID, PSERV or USER_DATA2 must hide fresh checkout availability while preserving
the existing credential-based status query configuration. This does not permit
a new attempt, fallback, capture or paid state without the original verification
and immutable payment binding.

Evidence consists of local Hspec parameter and form-decoding cases, plus the real
SQLite-backed public availability handler with each required metadata field
removed and a still-usable status configuration. There is no provider sandbox,
3DS, browser-widget, certification, production-charge or all-domain refinement
claim. Provider outcome, concurrency and idempotency retain their existing
contracts; no new lifecycle model or activation authority is introduced.

The provider's public integration guide is
https://developers.datafast.com.ec/Index.aspx (read 2026-10-29). TDF's private
onboarding email and attachments remain outside Git. Merchant certification is
still blocked on onboarding, issued test credentials, applicable buyer/cart/tax
fields and actual credentialed qualification. Do not infer tax allocation from a
gross total or replace missing buyer data with fictitious defaults.

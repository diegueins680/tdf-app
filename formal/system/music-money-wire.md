# Exact music monetary values

`PAY-MUSIC-MONEY-WIRE-001` covers availability authoring/read projections,
purchase confirmation and purchase/provider response amounts. Music sales support
USD, as required by the existing content validator. This contract preserves the
nonnegative Int64 minor-unit domain; it does not impose an arbitrary price cap or
promise that a provider accepts every representable price.

## Wire and display contract

JSON numbers are allowed only in the interoperable integer range
`[-9007199254740991,9007199254740991]`. Outside that range, monetary values use
canonical decimal strings with no whitespace, leading plus, leading zeros,
fraction, exponent or negative zero. The general codec covers signed Int64;
prices and purchase financial fields are separately constrained nonnegative,
and a purchase confirmation must be positive. Zero and absent/free prices retain
their existing meanings. An unsafe incoming JSON number is refused, never rounded.

The SQL function `music_minor_units_json(bigint)` and Haskell codec share this
boundary. The new additive migration creates only the pure encoding function;
it does not rewrite stored prices, immutable snapshots, financial evidence or
applied migration history. Public availability, private editable availability,
purchase lists/detail and provider checkout response amounts use the encoding.
Historical export snapshots retain their original JSON representation and hashes;
this contract does not assert lossless parsing of arbitrary exports by old clients.

Web authoring uses a text input with numeric input mode, so native binary64
spinner/arrow stepping cannot alter a large price. It keeps input as text. Its adapter emits a compatible numeric value
only when conversion is exact; larger values remain strings. Rendering uses
BigInt whole units and Intl parts; only the remainder in `0..99` becomes Number.
Unsupported sale currencies or already-lossy legacy numeric responses cannot
produce an actionable purchase. A string in a TypeScript declaration alone is
not runtime validation: the adapter and form validate the actual value.

## Confirmed purchase state machine

`new -> validated exact quote -> currently eligible USD offer -> accepted | conflict`.
Every purchase request must carry canonical `expectedAmountMinor` and
`expectedCurrency`. Missing confirmation fails decoding with 400; malformed or
nonpositive amounts and malformed currency fail with 400. A valid confirmation
that differs from the eligible offer returns 409 before an order, checkout,
terms record or provider action. The published offer remains the price authority;
client confirmation records agreement, not permission to choose a price.

Current feature/offer admission and buyer scoping still apply on replay.
`PAY-MUSIC-PURCHASE-REPLAY-001` continues to bind the buyer/key to the original
normalized request. The immutable published price and checkout financial snapshot
must not be edited to accommodate a retry. The existing unique reservation and
transaction boundaries remain in force. This does not prove atomic eligibility
against a concurrent withdrawal or repair provider transport uncertainty.

## Compatibility and recovery

Safe legacy numeric amounts remain readable and writable. New clients also
understand exact decimal strings. Older clients without exact quote confirmation
are refused by the new purchase API. The previous API rejects the new fields;
therefore the experimental commerce flag must remain off across frontend/backend
delivery and may be considered for activation only after the compatible deployed
pair and the other open commerce obligations are verified. No native purchase
caller was found at the shipped Mobile pin; generated Mobile types describe the
contract without activating a native purchase workflow. Later Mobile main has
other API changes and is not silently imported.

Keep the additive SQL function on application rollback. Do not restore financial
data or edit an applied migration. An older application would reintroduce unsafe
amount handling and must not run with music commerce enabled. No provider request
or real payment is needed for this verification.

## Verification boundary

- `TDF.MusicRelease.Money`, the SQL encoding function and the generated OpenAPI
  scalar schema are recognized by `scripts/verify-music-money-wire.py`.
  Z3 checks the full signed64 numeric branch: conversion through IEEE754 binary64
  with round-to-nearest and back preserves the admitted integer. Widening the
  bound, omitting its lower half or using binary32 must produce counterexamples.
  Unknown source shapes and mismatched Haskell/SQL/contract bounds fail closed.
  Controlled contract mutations remove or weaken the string branch and must fail.
  Decimal boundaries are checked in both Python and the actual JavaScript regex engine.
- That obligation trusts the source recognizer and numeric-parsing translation.
  It does not prove Aeson, PostgreSQL, decimal-text libraries, Intl, HTTP sessions,
  provider behavior or whole-program refinement. Real codecs are additionally
  property tested over full-range Int64 samples and explicit boundaries.
- Real HTTP checks on an owned disposable PostgreSQL fixture test authentication,
  buyer-scoped lists, exact public/purchase JSON, missing/malformed/mismatched
  confirmation and replay. A temporary lossy SQL-function mutation must fail
  the exact HTTP price oracle; the canonical function is restored in finally.
  Fixture sandbox flags are restored; no provider endpoint is invoked.
- Real-handler PostgreSQL tests check published and purchase JSON plus rejected
  price/currency confirmation and zero purchase effects. Adapter and component
  tests check exact submitted values, formatting, invalid legacy responses and
  an authored price through the actual form. Exact-revision receipts distinguish
  working fixtures from a canonical migration rehearsal.
- The broader music API/media integration fixture also applies the new encoder
  migration and sends exact confirmation on initial purchase and replay. Its
  previous selected-migration setup failed the first private version GET with500;
  this setup failure is not a passing monetary semantic negative control.
  The completed fixture also binds graph-corruption inserts and repair to its
  exactly one processed master for the cloned version/recording; uploaded
  historical masters are retained and all rejection/rollback assertions remain.
- The OpenAPI declaration documents purchase POST/GET and the generated client
  types are used by the web adapter. Other undocumented music operations remain
  a separate API completeness obligation.

The canonical backend entrypoint now executes these music specs under
`SYS-MUSIC-TEST-ENTRY-001`; the old uninvoked convenience runner is not CI evidence.

## Authority and research

PostgreSQL BIGINT and Haskell Int64 permit values that a JSON-number/Number
pipeline cannot preserve. The old "integer" type labels and the form's
"Sin flotantes" help text did not prevent conversion with Number.
[RFC 8259 §6](https://www.rfc-editor.org/rfc/rfc8259.html#section-6) identifies
the interoperable integer range. [ECMA-402](https://tc39.es/ecma402/2022/#sec-formatnumerictoparts)
specifies formatting parts for Number/BigInt. TDF adopts exact encoding and
BigInt display while retaining compatible small numeric values; no provider
architecture or unsupported currency policy is inferred from these sources.

# Compatible URI decoder — SYS-URI-DECODE-001

The canonical requirement is `SYS-URI-DECODE-001` in `requirements.json`.
The pinned Mobile dependency graph replaces the affected decoder0.2.2 with an
unchanged, integrity-pinned upstream0.5.0 algorithm and a private CommonJS adapter.
`query-string`7 expects a callable `require()` result; requiring the new ES module
without selecting its default export breaks that caller. The adapter also keeps
the old caller's literal-plus normalization and type-error behavior.

Both the adapter and upstream npm alias are explicit root Mobile dependencies.
The `$decode-uri-component` override directs transitive consumers to the same
adapter. This matters for clean installs: local linked packages do not install
their own dependencies, and a relative file override nested beneath a consumer
produced an invalid nested lock record during the initial experiment. The final
lock must resolve the root `vendor/decode-uri-component` path and reproduce with
`npm ci`; a passing already-installed workspace is insufficient evidence.

`src/lib/__tests__/uriDecoder.test.ts` in Mobile executes the actual installed
query-string caller, eleven boundary cases, non-string rejection and250 seeded
Unicode samples. For repeated `%FF` input with byte counts4,8,16,32, it observes
built-in decode calls and requires a positive count no greater than8N+8. The
actual licensed upstream0.2.2 test fixture fails that same oracle atN=32; an
unadapted0.5 namespace breaks the actual CommonJS caller in a second control.
This is a deterministic regression property over that input family, not a
universal complexity proof or a runtime request-size restriction.

The original0.2.2 fixture SHA256 is
`3b8ba0a765e1089d11bba9a919d0d8789ecd9a6833ea078d8d9114e1db093863`;
its bytes and license match the npm release. It lives only under Mobile's
`scripts/fixtures` and is never an application dependency. Upstream0.5.0's npm
integrity is recorded in the committed Mobile lockfile. No new audit exception
or claimed vulnerability-free distribution follows from this repair.

## Corrected reachability scope

Earlier dependency notes claimed current React Navigation/Expo query parsing
reached this decoder. Fresh inspection of the exact installed versions narrows
that claim: Expo Router6.0.24's incoming `parseQueryParams` uses
`URLSearchParams`; React Navigation core uses its own `queryString.js` parser.
Expo's two `getPathFromState` helpers import query-string7 for serialization.
The installed query-string7 `parse` API does call the affected decoder, but this
does not establish a currently exploited or reachable incoming app deep-link
attack. This change removes the affected dependency implementation and preserves
its consumer API. Native bundles, client gates and independent review remain
required; device/store behavior is a separate verification boundary.

Primary sources:

- https://github.com/SamVerschueren/decode-uri-component/security/advisories/GHSA-vcc3-ghjq-m6fr
- https://docs.npmjs.com/cli/v10/configuring-npm/package-json/#overrides
- https://nodejs.org/download/release/v20.19.4/docs/api/modules.html#loading-ecmascript-modules-using-require

# Event report link access

EVT-REPORT-LINK-001 implements the organizer's request for an in-application
PATCH CULTURE report visible to anyone who has the link.

The existing Cloudflare Pages application serves read-only HTML at
`https://www.tdfrecords.net/reportes/eventos/<capability>`. The capability consists
of 32 cryptographically random bytes in canonical 43-character base64url form.
Its SHA-256 digest selects the report; the decoded bytes are the AES-256-GCM key.
Only authenticated ciphertext, a random 12-byte nonce, version and digest are
committed. Version 1 authenticates the fixed context `tdf-event-report:v1` and a
128-bit GCM tag. The raw capability and editorial input are kept separately,
never in public source, test logs, menus, sitemaps, React bundles or static assets.

The edge worker authenticates and decrypts the authorized provisional snapshot
on each admitted request. It preserves the original figures, reconciliation
qualifications and pending items. It does not call accounting, payment, attendee
or event APIs and performs no writes. Public tests use fictional data and keys.

**Historical disclosure:** the earlier public PR revision contained the plaintext
snapshot, tests and explanatory figures. This forward encryption correction
removes that content from current files, but does not erase public Git history,
old deployments or copies already obtained. Historical confidentiality cannot
be claimed. The capability itself was not committed in those revisions.

## Access boundary

| State or request | Result |
| --- | --- |
| Enabled, matching complete capability and authenticated payload, GET | Read-only HTML, no login |
| Same conditions, HEAD | 200, no body |
| Missing, noncanonical, malformed or wrong capability | Generic 404 |
| Disabled, malformed configuration or corrupted payload | Generic 404 |
| Digest override without resealing | Cannot grant a new key access |
| New key and payload resealed for that key | Only the new complete capability works |
| Cookie, event ID, name or query-string token | Grants no access |
| POST, PUT, PATCH, DELETE or OPTIONS | 405, no mutation |
| Unrecognized report path | 404 before the SPA can load |

Success and denial responses carry private/no-store, CDN no-store, no-referrer,
noindex/nofollow/noarchive/nosnippet, nosniff and a restrictive CSP with no scripts,
forms, frames or external resources. Legacy production-host redirects for this
route use a noncacheable 302 with protective headers. The page uses the existing
TDF logo, Spanish content, a responsive accessible table and print stylesheet.

Anyone receiving or forwarding the capability can read this snapshot. Revocation
cannot remove saved copies. noindex is a crawler request; authenticated decryption
is the access boundary. Provider access logs may contain URLs: never publish raw
report paths in operational logs or analytics. This page loads no app analytics.

## Author, revoke and rotate

Store the approved payload JSON and a separate JSON object containing `token`
in private files outside tracked paths, both mode 0600. The payload follows the
validated schema in `event-report-crypto.mjs`, including every private editorial
explanation; do not hardcode figures or recipient details in the renderer.

```sh
node scripts/seal-event-report.mjs --input /private/path/snapshot.json --token-file /private/path/link.json --output functions/_shared/event-report-data.mjs
```

The offline command generates a fresh nonce and prints only sealing status,
version and public digest. Its errors suppress private input. Inspect the diff
for accidental plaintext or capability inclusion before publishing.

Set Pages binding `EVENT_REPORT_LINKS_ENABLED=false` and redeploy to revoke.
Only an absent value or exact string `true` enables the page. Apply revocation to
every deployed environment that served it. Immutable previews keep their own
configuration; retire old preview deployments separately.

To rotate, privately generate a new capability with Node's
`crypto.randomBytes(32).toString('base64url')`, reseal the approved payload with
that key, and deploy the new module. If `PATCH_CULTURE_REPORT_TOKEN_SHA256` is set,
update it to the new module's lowercase digest too. Updating that binding alone
cannot decrypt a payload sealed for an old key. Empty or malformed bindings fail
closed. Deliver the new URL privately after verifying the deployment. Do not use
`VITE_*` variables or commit input/key files.

## Verification

```sh
node --test scripts/__tests__/event-report-crypto.test.mjs scripts/__tests__/event-preview.test.mjs scripts/__tests__/ticket-page-preview.test.mjs
```

Independent Node/OpenSSL fixtures check Worker WebCrypto interoperability,
authentication failures, noncanonical keys, tampering, context binding, malformed
payload denial, fresh nonces and sanitized CLI failures. Request/Response tests
check access, revocation, complete resealing on rotation, digest-only denial,
methods, escaping and middleware. This finite matrix is not a cryptographic
proof or proof of provider cache/log behavior.

Compile Pages Functions with Wrangler, then inspect actual hosted HTTP responses
and desktop/narrow browser views. Vite does not execute Pages Functions. Follow
the protected web release process and verify the deployed commit before calling
the canonical URL live. No API deployment or database migration is required.

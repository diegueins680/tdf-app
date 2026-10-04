# Instagram Login lifecycle validation

## Why the implementation changed

Meta Support [case 1978991836117141](https://developers.facebook.com/support/bugs/1978991836117141/) confirms that Facebook Graph `/debug_token` does not reliably support Instagram Login tokens and that no equivalent documented read-only introspection API exists. Changing the parent Facebook credentials is not a fix.

The supported [Business Login lifecycle](https://developers.facebook.com/documentation/instagram-platform/instagram-api-with-instagram-login/business-login) exchanges a one-use authorization code with the Instagram app ID/secret, then exchanges the resulting short-lived token for a long-lived token. Meta returns its lifetime as `expires_in`. Refresh requires a valid long-lived token at least 24 hours old and the basic permission. This implementation records that evidence when the provider supplies it; it never infers an issuance date from a legacy secret's update time.

## Evidence and trust boundary

The checker requires all of:
1. An authenticated encrypted checkpoint bound to this repository and Instagram app.
2. A token fingerprint matching the checkpoint's credential.
3. Ownership provenance from a successful authorization-code exchange using the configured app ID and secret.
4. A provider-returned positive, bounded lifetime and consistent issuance/expiration arithmetic. Expiry is measured from request start; minimum refresh age is measured from response receipt so network latency cannot make a young token eligible early.
5. Any previously supplied data-access deadline remaining valid.
6. Live `/v26.0/me?fields=user_id` access for the same Instagram account.

OAuth code exchange must return the basic permission and an app-scoped user ID.
The short token's `/me?fields=id,user_id` response must bind that grant ID to
`id` and the explicitly pinned professional account to `user_id`. Meta's
[Get Started field definitions](https://developers.facebook.com/documentation/instagram-platform/instagram-api-with-instagram-login/get-started)
distinguish these namespaces; they must not be compared directly. Both IDs are
preserved in authenticated authorization evidence. Permission names in the
checkpoint are the grant-time evidence, not a claim that every scope has been
exercised now. Missing provider data-access metadata is recorded as absent, never
fabricated as a deadline or claimed to be never-expiring. Live access checks
detect revocation; a known data-access deadline is preserved across refresh and
never silently extended or dropped.

Provider `user_id` values may be JSON integer literals beyond JavaScript's safe
integer range. Node 22's source-aware JSON reviver retains their exact digits;
converting an already parsed number to a string is unsafe. Fractional, negative
and exponent-form identifiers are rejected. Numeric lifetimes remain numeric and
retain their separate strict validation.

An existing raw `INSTAGRAM_ACCESS_TOKEN`, a legacy plaintext state file, a working `/me` response, or a manually authored approval record cannot bootstrap this evidence.

## Configuration

Repository Actions secrets:

| Name | Scope/purpose |
| --- | --- |
| `INSTAGRAM_APP_ID` | Existing Instagram Login child application ID. |
| `INSTAGRAM_APP_SECRET` | Existing matching Instagram secret; authenticates exchange and derives the checkpoint encryption key. |
| `INSTAGRAM_AUTHORIZATION_CODE` | Fresh one-use code from the approved login flow, used only by the setup step. Never put it in dispatch inputs, command-line arguments, logs or PR text. |
| `SLACK_WEBHOOK_URL` | Optional failure notification; absence does not mask failure. |

Repository Actions variables:

| Name | Purpose |
| --- | --- |
| `INSTAGRAM_REDIRECT_URI` | Exact registered HTTPS callback used for the authorization request; only needed during setup. |
| `INSTAGRAM_USER_ID` | Intended Instagram professional account ID (`/me.user_id`), explicitly checked during bootstrap and subsequent checks; not the OAuth grant's app-scoped ID. |

No Facebook inspector credential, Fly credential, GitHub secret-write token, or production deployment permission is passed to this workflow. The GitHub token has only contents/read and actions/read. The encryption context is `GITHUB_REPOSITORY` (or explicit `INSTAGRAM_LIFECYCLE_CONTEXT=owner/repo` for local use).

## Approved bootstrap

### Verified repository configuration (2026-09-15)

The attached Meta developer console for parent app `1098715965613487` lists
`https://tdf-app.pages.dev/oauth/instagram/callback` in the Instagram Business
Login OAuth redirect allowlist and in its generated authorization link. The
Instagram child app is `1206294904899273` (TDF Bot-IG). The intended account is
`tdf.records.label`; the earlier successful live account verification recorded
Instagram user ID `17841445628242005`. Bootstrap still validates that ID against
the new provider grant; this record is not a substitute for that validation.

Do **not** use the normal production application's login/exchange flow to obtain
CI-only state. `useInstagramCallback` automatically submits codes after a matching
session-state check, and `ServerInstagramOAuth` persists connected-account tokens.
Using this registered URI for an isolated bootstrap requires a secure callback
receiver/interceptor that validates its own unpredictable OAuth state and prevents
the callback request from reaching the production application. Confirm that
interception before requesting authorization; fail closed if it is unavailable.
The callback registration was inspected without changing or saving Meta settings.
The subsequent operator-only bootstrap used a separate attached-browser tab,
bypassed service workers, and intercepted every request to the callback's origin
at the request stage. A harmless probe verified a locally fulfilled response
before authorization. The actual callback was also fulfilled locally, its fresh
256-bit OAuth state was checked, and the one-use code was transferred directly to
the Actions secret through process input, without logging or disk storage. Only
that helper-owned tab was closed. This verifies the isolated receiver, not the
success of any later provider exchange; workflow results must establish that.

1. Verify the intended Instagram app/account and a registered callback that does **not** automatically write production credentials. Do not change production callback behavior as an incidental CI fix.
2. Complete the approved Business Login authorization flow. Use and validate OAuth state at its callback. Store only the returned code in the repository secret through a secure operator interface; do not copy a callback URL containing the code into an issue or chat.
3. Configure the callback URI and account ID variables. Code exchange validates the configured app ID/secret, callback and account. A consumed/expired code fails; it is not automatically retried.
4. Dispatch the existing workflow on the reviewed branch:

```sh
gh workflow run refresh-instagram-token.yml --repo diegueins680/tdf-app --ref REVIEWED_BRANCH -f action=setup
```

The workflow exchanges the code, exchanges the short-lived token, verifies the resulting account, and uploads **only AES-256-GCM authenticated ciphertext** as `instagram-lifecycle-v1/state.enc.json`. It does not update the legacy repository token or any runtime secret and performs no deployment. After success, remove the consumed authorization-code secret through the authorized secret-management interface.

Missing configuration or unavailable user consent is a real bootstrap blocker, not a successful skip. Mock tests demonstrate program behavior, not successful provider provisioning.

## Read-only checks and approved refresh

```sh
gh workflow run refresh-instagram-token.yml --repo diegueins680/tdf-app --ref REVIEWED_BRANCH -f action=check
gh workflow run refresh-instagram-token.yml --repo diegueins680/tdf-app --ref REVIEWED_BRANCH -f action=refresh
```

`check` downloads/decrypts an eligible checkpoint, validates evidence and checks live account access. It makes no refresh, state, repository-secret or runtime writes. The existing schedule remains read-only (the cron selects the 1st and, when present, 31st at 03:00 UTC, not an exact 30-day interval).

`refresh` is explicit credential maintenance. It first verifies the existing evidence/live account and the 24-hour minimum age, obtains the new provider lifetime, verifies the returned account and persists a new encrypted checkpoint. The previous known data-access deadline and original OAuth ownership provenance are retained. Neither action deploys or updates Fly. Operators must run approved refresh before expiration; the workflow warns within seven days of its effective deadline and fails after expiration.

## Persistence and recovery

- Encryption uses a random 96-bit nonce with AES-256-GCM and a domain-separated HKDF-SHA256 key derived from the existing Instagram app secret. Repository/app identifiers are authenticated context. Token, fingerprint and evidence remain inside the ciphertext.
- Writes use a new private temporary file, fsync and atomic rename; plaintext token/state is never written to disk by this lifecycle. Local default `.instagram-lifecycle-state.enc.json` is ignored by Git.
- An encrypted artifact has 90-day retention, longer than the accepted maximum token lifetime. A successful check does not re-upload or extend retention.
- State discovery examines setup/refresh runs before artifacts. Only this workflow, this repository and a producing commit equal to or ancestral to the evaluated commit are eligible. Unmerged sibling-branch state cannot become main's credential state.
- GitHub responses are projected by `gh --jq` before subprocess capture: ancestry uses only comparison status, while run/artifact inventories retain every provenance and expiry field. Large unrelated commit patches therefore cannot exhaust the 1 MiB output limit. The 30-second request timeout, bounded pagination and fail-closed validation remain unchanged; metadata lookup failures do not imply that OAuth credentials need replacement.
- The newest eligible producer must have succeeded and retain exactly one unexpired checkpoint. Failed/canceled producers, deleted/expired artifacts, authentication errors and corrupt state fail closed instead of resurrecting an older token.
- Concurrency is serialized without canceling in-flight credential maintenance. Upload failure remains workflow failure. A rerun of a consumed setup code is not a recovery procedure.
- If persistence fails after a provider mutation, retain the failure evidence and use a new approved authorization bootstrap; do not fabricate a receipt or extend old expiry. Rotating the app secret invalidates existing ciphertext and requires a planned bootstrap.
- Merging a reviewed producer commit preserves its ancestry, allowing main to consume its authenticated checkpoint. Squash/rebase changes ancestry and therefore requires a new approved bootstrap. No ancestry exception is provided.
- Reverting code does not undo a token exchange. Production credentials remain untouched by this workflow. A later runtime promotion requires a separate reviewed deployment/secret-management operation.

## Local validation

```sh
npm run test:instagram-token-workflows
npm run quality:repo
```

Local CLI commands remain `--check`, `--setup`, and `--refresh`. Supply configuration through a secure environment, never argument values. `INSTAGRAM_LIFECYCLE_STATE_FILE` selects input ciphertext; `INSTAGRAM_LIFECYCLE_OUTPUT_FILE` selects the output ciphertext for setup/refresh. With no override, both use the ignored local default. The CLI's interface is preserved, but legacy raw-token adoption and implicit Fly updates are intentionally rejected/removed.

Tests cover provider response validation, OAuth ownership/account binding, tampering/context mismatch, expiry/data-deadline enforcement, retries/redaction, read-only checks, refresh age, atomic persistence and newest-checkpoint failure handling. They retain the independent Facebook messaging invalid-token regression. No test uses real credentials.

## Messaging token checks (separate workflow)

`Check Messaging Token` manages `INSTAGRAM_MESSAGING_TOKEN` and
`FACEBOOK_MESSAGING_TOKEN`, not the Instagram Login lifecycle checkpoint above. Its manual
`action=check` is read-only:

```bash
node scripts/check-messaging-token.mjs --check
gh workflow run check-messaging-token.yml \
  --repo diegueins680/tdf-app --ref REVIEWED_REF_WITH_READ_ONLY_CHECK -f action=check
```

Replace the ref placeholder only with a reviewed revision containing this fix.
This read-only change was merged into main as `6f67081a73e66e422f331cb31c455a78977a400e`.
Do not use older revisions: their manual `check` action runs credential
maintenance and their CLI silently ignores the flag.

Both tokens must pass the existing health checks without needing maintenance.
Missing, invalid, expired, soon-expiring tokens or failed provider checks return
a nonzero status; read-only mode never exchanges tokens, retrieves replacement
Page tokens, or calls Fly. In Actions, this step receives only the messaging
tokens and Meta inspector credentials; Fly credentials and CLI installation are
limited to maintenance. Failure notifications retain their existing behavior.

The hourly schedule and explicit `action=refresh` retain the existing
refresh-when-needed behavior, including final verification and failure exits.
They invoke `node scripts/check-messaging-token.mjs` without arguments, which can
exchange tokens and update both Fly messaging secrets. Manual `refresh` is not
an unconditional rotation and requires separate production-maintenance approval.
Unknown CLI arguments and workflow actions fail instead of falling through to
maintenance. Do not pass token values as arguments or paste them into logs.

No schema migration or application deployment is needed for this change.
Reverting it restores the old, potentially mutating manual `check` behavior;
stop using manual checks on a reverted revision. Reverting code does not undo
any separately authorized credential update.

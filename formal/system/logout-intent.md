# Explicit browser logout intent

`AUTH-LOGOUT-001` addresses an isolated native reproduction: aborting the logout
POST showed `/login`, but reloading restored the same valid server cookie without
new credentials. A successful local UI transition is not proof of revocation.

Before clearing the local session, persist an identity-free, versioned intent.
While it is active, do not adopt a stored descriptor or automatically request a
cookie snapshot. The shared API client omits cookies and application authorization
headers, and the two credentialed upload transports reject before XHR dispatch.
Public reads remain available unsigned. Existing cookie-only restoration and
uploads remain available when no explicit logout intent exists.

The existing response deadline bounds the single revocation request; include an
available application bearer and the HttpOnly browser cookie without extracting
or logging the cookie. Only a successful response confirms server revocation.
Network, timeout and authorization errors remain unconfirmed. Retain an available
bearer in memory only for explicit revocation retries, discard it on confirmation
or another authentication occurrence, and never expose it through session getters
or persistent storage. With no readable bearer, verify a valid cookie target
before POST: an empty-target 204 cannot certify revocation. A lost target after
reload remains unconfirmed. One total 30-second deadline covers both lookup and
POST; the lookup never adopts a session. Display that
uncertainty and offer explicit retry. Reloading a pending intent without its
original request treats confirmation as unavailable. No automatic revocation
retry may accidentally target a later user's cookie.

Fresh interactive authentication waits for this document's outstanding logout,
then clears the intent on adoption of the fresh result. A new logout revision
also cancels credentials still queued before dispatch or a retry. Every production session
adoption path either checks its captured logout revision (password, Google,
signup, password reset and invitation redemption), or both its existing synchronous
account epoch and captured revision (artist onboarding). A later intent invalidates earlier callbacks.
An old logout completion cannot overwrite a new login revision. Other tabs drop
their local account on any unseen authentication revision, including a cleared
revision from another tab's fresh login. A logout followed by login before storage
event delivery must still discard the old account and its query cache. They do
not silently adopt the other tab's fresh login. Session query caches retain their existing epoch isolation.

Storage corruption or unavailable writable storage disables automatic cookie
restoration. An explicit fresh login may operate in memory. This safeguard needs
the application origin's browser storage; deleting site data deliberately removes
its history. A writable-storage probe contains no personal data.

Limits: this is a browser application safeguard, not proof of server revocation
while offline. A copied still-valid cookie can authenticate directly until the
server revokes/expires it. Accepted requests, browser Set-Cookie ordering across
in-flight requests/documents, other clients, native mobile, external providers,
full distributed linearizability and physical-device acceptance are not proved.
Raw transports outside the scoped shared client and upload paths are not newly
certified. No backend, schema, provider or feature flag is changed.

Verification: focused real-provider regressions, storage/revision/late-response
controls, request deadline and unsigned public-read tests, plus optimized native
browser/API/disposable PostgreSQL rehearsal. Preserve pre-fix reproduction and
exact source identities. Protected checks, independent review, integration and
exact deployed-version verification remain separate gates. Human consent and
pilot targets remain unverified; automated fixtures never count as participants.

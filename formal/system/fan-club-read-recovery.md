# Fan-club read recovery

`COMM-CLUB-001` covers the existing web club route. An HTTP404 can establish
that the requested readable club is unavailable. A timeout, network error,
HTTP503 or authorization failure cannot establish that the artist has no club.
Show a retryable read failure and preserve the requested artist and deep link.
Never display raw API diagnostics. For401/403, the existing login route keeps
the pathname, search and hash. A successful cached club remains visible when a
background refresh fails.

A missing club offers the existing public `/artista/:id` page and Comunidad.
This is navigation, not private-club access or an assertion that media exists.
Invalid/nonpositive/unsafe IDs do not fetch another club. Content queries wait
for a valid readable club; the server remains responsible for authorization.

No read or fallback creates a club, follow, member, post, message or notification.
New artists still need a separately designed and authorized club activation
capability. Issue606 remains open for that part of the engagement loop.

The actual-component regressions distinguish loading, missing, failed, retry,
authentication and cached-data behavior. The pre-change page must fail the
recovery controls. Isolated browser/API/PostgreSQL receipts and protected
integration/deployed-version evidence are distinct from component tests. They do
not establish human comprehension, physical-device access or real-user value.

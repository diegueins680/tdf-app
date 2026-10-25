# Drive uploads preserve the authenticated browser session

`MEDIA-DRIVE-002` covers the web `uploadToDrive` transport used by the artist
cover editor. A browser restored from an HttpOnly cookie can be authenticated
without a JavaScript-readable bearer token. For a configured API on another
trusted origin, XHR must opt into cookie credentials, as the existing inventory
upload already does. Use the shared API resolver so deployment fallback and
normalization remain consistent with other authenticated requests.

A selected upload sends its multipart file and optional folder, name and provider
token unchanged. A readable application bearer remains an additional header when
present. The server remains responsible for session, origin, ownership and Drive
provider authorization. The client does not extract HttpOnly cookies into storage,
copy them into a bearer, or log credential values.

A successful response preserves the existing Drive DTO conversion. A rejected
upload remains a rejected promise and the artist editor retains its existing
explicit retry behavior. No automatic retry or exactly-once storage is added.

Verification combines cookie/bearer/resolver regression tests with a native
cross-origin browser check using a real disposable API/PostgreSQL session and a
non-production Drive adapter. The adapter checks whether the browser actually sent
a cookie. An omission mutation must fail. This verifies transport credentials;
it does not certify Google OAuth, Drive permissions/storage, production cookies,
physical devices or user comprehension. Release still requires current protected
checks, independent review and deployed-version verification.

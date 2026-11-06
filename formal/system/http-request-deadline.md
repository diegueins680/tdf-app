# Web HTTP response deadline

## SYS-WEB-REQUEST-001

The web auth transport and shared API client keep one 30-second deadline from
request start through the response data they consume. Receiving headers does not
finish a request. Native Fetch aborts a pending body reader when its request
signal is aborted. This boundary depends on that platform behavior; it is not a
proof that arbitrary Fetch replacements obey cancellation.

- A deadline failure becomes the existing translated auth timeout code or API
  timeout status 408. Pending global API activity returns to zero.
- Caller cancellation remains cancellation, including after headers. An already
  cancelled request does not start, and a cancellation racing with completion
  cannot publish its result.
- A disconnected body fails as a network error. It never becomes an empty success.
- The deadline and caller listener are removed after success, failure or abort.
- Login and Google login retain the existing four-attempt limit for transport
  failures **before** headers and explicit HTTP 503 responses, including existing
  Retry-After handling. A failed body read is not replayed: account creation or
  authentication may already have taken effect.
- Signup, reset initiation/confirmation and general API writes have no automatic
  replay. A client timeout is not evidence that the server rolled back a write.
  Existing domain idempotency/reconciliation remains authoritative.
- Successful void reset initiation does not need a body; API 204/205 responses
  remain empty successes. Existing JSON/text result handling is preserved.

Deterministic fake-clock tests cover success/error body stalls, the combined
headers/body budget, post-header cancellation, pre-cancellation, interrupted
bodies, login retry bounds and no reset/signup replay. Session-cache tests retain
late-401/account-isolation coverage. These are executable transport regressions,
not universal proof, physical Redmi testing, deployment or provider qualification.

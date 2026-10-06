# Social message transport boundary

`NOTIF-TRANSPORT-001` covers one Instagram, Facebook or WhatsApp text/template
transport invocation. A valid request selects its provider account and token
before dispatch. Explicit Instagram context must be complete; it never falls
back to the platform account. The configured account is selected only when no
explicit context was supplied. The old `/me` and alternate-token fallback chain
is retired: credentials which only worked through that fallback now require
operator correction, not another ambiguous POST.

A message POST does not follow redirects. The production constructors use a
shared messaging-only TLS manager whose `managerRetryableException` is always
false. This matters even without application retry loops: pinned
`http-client-0.7.19` retries a request after some errors on a reused connection,
without checking its method. Unrelated read/enrichment managers remain unchanged.
WhatsApp's environment and enrollment-service constructors both use these settings.
The exported manager-injected helpers and low-level WhatsApp client require an
appropriately configured trusted manager; they are not an untrusted HTTP interface.

Transport exceptions return a content-free unknown-outcome error. They do not
render the request, bearer token or recipient. Asynchronous cancellation escapes
these three adapters. HTTP errors retain the existing provider-response handling;
none authorizes automatic fallback. A cancellation, connection loss or error
response does not prove that the provider did nothing.

The local test suite drives actual owned loopback servers using production
messaging settings with only destination routing/proxy settings overridden. It
covers each channel's accepted-body/lost-reply case, redirect refusal, cancellation,
and a warmed persistent connection followed by acceptance and EOF. It counts
requests, including a possible hidden library retry. Negative controls add an
explicit repeat, restore redirects, swallow cancellation, and restore default
connection retries; each must fail its corresponding behavioral assertion.
Static correspondence checks also retain the four production manager bindings.
All messages, credentials and recipient identifiers in these fixtures are synthetic.
The fixture does not verify provider TLS certificates or provider behavior.

This boundary is not exactly-once delivery. User/API retries, restarted workers,
parallel consumers, SMTP, provider-internal duplication, caller-side exception
handling and durable queue claims remain separate obligations. A successful
transport response retains its existing parsing semantics and is not a new
provider reconciliation guarantee. Social/course background admission stays off
pending those repairs; this component does not activate any worker or provider.

Primary source: the pinned HTTP client implementation at
https://github.com/snoyberg/http-client/blob/http-client-0.7.19/http-client/Network/HTTP/Client/Core.hs
and RFC9110 section9.2.2. We adopt explicit no-retry transport behavior because a
message send has no demonstrated provider idempotency binding.

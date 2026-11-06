# Client failure diagnostic privacy

## PRIV-ANALYTICS-001 and SYS-WEB-SHELL-001

Application error reporting must not export an original Error object, its stack,
cause or custom account fields to console collectors or native SDK exception
capture. The routed section boundary uses the same `client_error` reporter as
boot/auth failures instead of sending a complete location URL and raw exception.
Recovery controls are unchanged.

The reporter redacts names, messages and the explicitly selected diagnostic text
fields. It masks emails, recognized credential labels, JWT/opaque identifiers and
embedded HTTP/HTTPS/tdf or relative URL/path strings, including short query and
fragment values that a URL-property sanitizer cannot recognize inside prose.
Recognized credential labels include bearer/authorization, access/refresh/ID
tokens, current/new passwords, API keys and client secrets with common camel,
underscore and hyphen spellings. Quoted values, including whitespace and escaped
quotes, are masked as a unit; unterminated quoted values are masked to the end.
Selected boolean flags retain their actual type. Unknown context properties are
dropped; context cannot replace the event kind, error fields, current path,
revision, network state or document visibility. Standard analytics serialization
continues to sanitize SDK URL envelopes and private route IDs.

Only the redacted error name/message are passed to the application's console
call. No stack, cause or arbitrary error fields are logged there. A throwing
error getter or unavailable console does not make reporting throw. Provider
capture remains asynchronous and best effort; deduplication and the page report
cap remain in force.

This is a scoped application-diagnostics boundary. Pattern matching cannot
recognize arbitrary personal prose. Callers must still avoid form/account data
in diagnostic messages and allowed context fields. Browser/React/third-party
logging, other application logging paths, provider retention and erasure of
historical events are outside this repair. No provider credentials or production
payloads are used by the synthetic regression tests.

Validation covers sentinel exclusion from outbound captures and application
console arguments, unknown/reserved context keys, embedded URLs with short
secrets, custom error names, throwing getters/loggers and the actual routed
boundary-to-reporter integration. These are executable tests, not a universal
privacy or whole-application logging proof.

Regression coverage also runs the routed boundary through the real shared
reporter, analytics client and SDK `before_send` envelope using synthetic data.
Negative context injections cover kind, name, message, path, revision, network
state and visibility; retry controls are exercised against a rendered boundary.

# Event memory ownership at the synchronous session boundary

Authority: the event-community request of 25 October 2026, privacy, authorization,
explicit communication consent and resilient drafts. Scope: the web client.

A credential epoch advances synchronously during login/logout/replacement before
React necessarily renders a new account. A captured form callback must already
be invalid then. Reading the new epoch only inside mutationFn wrongly adopts the
new credentials for an old draft, preference or moderation decision.

Reuse the existing owned-profile editor authority as shared useSessionOwner and
useOwnedSessionDraft. Preserve the profile entry point as an export facade and
its loaded-baseline/dirty-edit/acknowledgement behavior. Identity includes the
Party owner, credential epoch and optional event/editor scope. An old setter or
callback cannot regain authority when a new render or source scope arrives.

Each publication attempt captures the event, draft, capability snapshot and owner
guard at the user action. Require that guard before upload, after upload and before
create. Cache uploaded references only for that scope/file. A new account sees an
empty draft synchronously; late prior success cannot erase its draft or phase.
Edits/deletes also capture their action/event/owner. Source action dialogs retain
the opening owner across retries, including before request and after acknowledgement.

Preference/campaign mutations capture the owner with their value, event and cache
keys. No old action can opt the new account in; a late old response cannot replace
its preference cache. Existing defaults and server eligibility/consent stay intact.
Requests already committed under the original account are not retroactively undone.

Observed counterexamples: three pre-render synthetic tests each dispatched one
request on the old implementation (publication, preference, moderation). The same
tests must dispatch zero after correction. Retain successful publication and retry,
late-response, scope-change and existing real-provider/profile regressions.

This is executable regression evidence for the client session epoch and scopes,
not a proof of all browser credential mechanisms, all tabs, devices or server
linearizability. Server authentication, current source authorization, parent access
and request-ledger policy remain independent obligations. Native behavior and the
separately owned failed-logout repair are not established by this contract.

The real SessionProvider, imperative bearer credentials, preference API and HTTP
client are also exercised with a synthetic fetch boundary. An old checkbox is
invoked during a batched A-to-B login; no consent write may use B credentials.
A separate explicit B click must produce one correctly authenticated write.

Publication/edit acknowledgements also need a persisted source ID, compatible media
shape and current event/source binding when present; legacy replies without an
eventId remain accepted. Nonempty HTML is not a delete acknowledgement. Retain
the editor on uncertain replies, and advise checking the event before retrying.
This does not implement general creation idempotency or prove commit from a reply.

Preference and campaign replies must have their typed boolean/aggregate shape; a
save must acknowledge the requested value. Malformed HTML or an unchanged opt-in
is not successful unsubscribe. Attribution accepts only a structured recorded
boolean and never invents a new opening. No preference changes on a failed reply.

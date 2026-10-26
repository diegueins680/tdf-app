# Follow response recovery

`COMM-FOLLOW-RECOVERY-001` covers the web public artist and Comunidad actions.
A lost HTTP response does not establish that a follow or unfollow failed. After
an ambiguous write, read the existing account-scoped membership endpoint once.
Never replay the POST/DELETE automatically and never treat a stale cache as
confirmation. A successful read displays current truth, including an unchanged
relationship when the request never committed. An unreadable state remains
unknown; the existing read retry repeats only the GET and disables blind toggles.

Comunidad retains the existing `isFan` capability, which covers Fan and Customer
roles, for membership reads, actions and explicit read retries. Retrying the hub or an owned
artist profile never refetches fan-only profile/membership resources for an
Artist-only or Admin-only account. Cached membership data cannot enable an action for
an account lacking that capability. Backend authorization is unchanged.

The command captures the exact session occurrence and safe positive account and
artist identifiers. Guard before sending and before returning the GET result to
QueryClient; a late response cannot populate or confirm another occurrence.
The existing SessionProviders cache isolation and backend authorization remain
authoritative. Pending writes and read recovery coalesce repeated clicks. A
profile receipt can clear follow intent only in its owned profile lifetime,
preserving unrelated search parameters and the hash. A delayed callback may
rewrite the URL only while its captured navigation key remains current. If the
reader navigates within the same artist page during recovery, retain that newer
URL; the existing resume effect can clear only its current valid intent after
membership is confirmed.

A confirmed desired follow can invoke the existing onboarding completion
handshake. That endpoint requires actual server event evidence; only its
`newlyCompleted` receipt can emit first-value analytics. A GET alone is not a new
activation, and neither a pre-existing pair nor fixtures qualify as VAR. This is
not a durable exactly-once analytics receipt implementation.

Actual helper and component controls cover committed follow/unfollow response
loss, unchanged state after a pre-commit failure, failed reads and explicit retry,
repeated clicks, invalid evidence, clock rollback and account switches during a
read. Pre-change component and real API/PostgreSQL counterexamples distinguish
implementation defects from simulated HTTP failures. The isolated browser drops
a real successful response only after the API commits; retain both actual source
identities and SQL assertions. Browser evidence, protected integration, exact
deployed-version evidence and consented human outcomes are separate obligations.
The existing session-cache model retains its scope and exclusions; no new
universal refinement or concurrency proof is claimed here.

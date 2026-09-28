# Scoped task/RACI web read contract

TV-01–06 refine the existing TA/TR read boundaries; no new task domain or write operation.
Subview: `/social/eventos/:eventId?tarea=:activityId`, linked from the existing logistics plan.
Use the existing registered event-detail route, with a separate mounted task-only component;
do not mount the ordinary overview and its event/moments/ticket queries for this subview.
An empty or repeated `tarea` parameter is invalid, not a fallback to the overview.
`SocialEventWorkspacePage` dispatches before lazily importing either reader, so the task path
does not import the ordinary overview's dependency tree. Registry metadata stays unchanged.
The server's event-operations feature flag remains disabled by default. The link grants no
authority; feature-disabled, absent and forbidden responses expose no task fields.

| ID | Obligation | Model / executable refinement |
|---|---|---|
| TV-01 | Fetch only the exact canonical task, not its parent event, plan, profiles or directory | `TaskRead` / `TaskReadStructure`; API/component call assertions |
| TV-02 | Show only validated current-context data; clear old content synchronously on target, session, credential or refresh generation change | `TaskView.CurrentView`, late/retained negative configurations; rendered navigation/session races |
| TV-03 | Bind bearer requests to the captured session, cancel on cleanup and ignore old successes/errors | `TaskView` generation abstraction; explicit optional bearer/signal API options, component tests |
| TV-04 | Require canonical positive safe decimal IDs and existing strict task DTO decoding; no sensitive response diagnostics | `TaskView.ValidatedView`, invalid negative configuration; malformed route/wire tests |
| TV-05 | No query cache, persistence, offline data, optimistic success or mutation; refresh drops prior data before dispatch | Local component state only; API no-store; refresh/failure/unmount tests |
| TV-06 | Spanish default, English fallback, semantic RACI table, visible loading/error/retry and honest policy/attention labels | Locale resources, rendered accessibility and browser checks |

`TaskView`: 3 context generations, 2 request slots, 2 abstract event/task targets, two
accounts plus logged out. Same-account credential rotation, logout/login, navigation and
explicit refresh advance generation. Valid response abstracts transport success plus exact
DTO/target validation; failure never becomes displayed data. No fairness/network liveness
claim; the shared HTTP client already bounds requests to 30 seconds. The model must pass
with the existing full formal suite before feature code is added. Negative controls must
fail with named invariants, not parse errors.

The API exposes IDs, status, activity/policy versions, canonical RACI and an accountability
attention flag only. Do not invent titles, names, dates, dependency lists, readiness, or an
aggregate write version. Display party IDs without fetching restricted personal data.
Development StrictMode may start and abort one initial read before starting the mounted
read. GET is side-effect-free at this boundary; each explicit refresh starts one new read.
Cleanup prevents the aborted request from publishing a receipt even if the transport resolves.
Absence of policy means no task policy recorded, not permission to complete. A false attention
flag does not establish dependency readiness or authorization to mutate.

Session generation fencing protects this view, not every existing logistics query or the
application's shared auth-expiration notifications. Cookie-only transport uses existing browser
credentials; JavaScript cannot pin an HttpOnly cookie or prove cross-tab identity. Authorization
is re-evaluated by the server on each GET; remote grant changes are not pushed into an already
displayed receipt. Users can refresh; no continuous revocation guarantee is claimed.

No backend/schema/client-generation change, no permission expansion, no provider activation.
Rollback the web route/link and optional API options together; old task callers remain compatible.
Task commands, rich fields, search/list APIs, aggregate concurrency, event overview, logistics
templates, native mobile and full-stack browser verification remain separate increments.

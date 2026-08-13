# Feature-discovery telemetry and privacy

Events use stable feature IDs and platform/source metadata:

- `feature_navigation_selected`
- `feature_search_no_results`
- `feature_search_result_selected`
- `quick_create_selected` and `quick_create_locked_selected`
- `feature_favorite_changed` and `feature_pin_changed`
- `feature_access_request_submitted`, `feature_access_request_reviewed`, and `feature_access_request_cancelled`
- `feature_403_shown` / `feature_403_viewed`
- `feature_destination_unresolved`
- `locked_feature_selected`

No raw discovery query is recorded. The no-results event records only locale and query length. Events never include access tokens, names, emails, record content, justification text, production record identifiers, role arrays, or module arrays. Access requests record exact feature/action but not requester identity in analytics; the transactional backend audit retains the authorized actor reference under existing audit controls.

Suggested dashboards:

- Authorized primary features with low navigation selection by user-type cohort.
- No-result rate by locale and query-length bucket.
- Locked selection → access request conversion by feature ID.
- Destination-unresolved and 403 rate by revision/platform.
- Quick-create selection and completion by feature ID.
- Favorite/pin use and stale-permission filtering.

Retention and access should follow the existing analytics policy. Feature discovery must remain separate from searches over people, messages, contracts, and business records. Any future query-content capture requires a separate privacy review and explicit minimization design.

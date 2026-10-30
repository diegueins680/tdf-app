# Weekly platform measurement

Status: implemented offline preparation; production export and SQL execution unverified.

Generate a bounded, read-only PostgreSQL aggregate query from the actual model
fields `UserOnboardingProgress` and `ArtistProfile` in `tdf-hq/src/TDF/Models.hs`:

```sh
python3 scripts/weekly-platform-metrics.py --cutoff 2026-10-30T08:40:00-05:00 > /private/new-query.sql
```

An authorized database operator runs that query through an existing approved
read-only connection, using `psql -XqAt -v ON_ERROR_STOP=1`, and privately exports
only the single JSON row. No credential or connection URL belongs in the report.
The tool itself never opens a database or network connection. The production
inspection role's table grants have not been verified; a permission failure is a
blocker, not permission to broaden those grants or switch credentials.

```sh
python3 scripts/weekly-platform-metrics.py --cutoff 2026-10-30T08:40:00-05:00 --input /private/aggregate.json
python3 scripts/test-weekly-platform-metrics.py
```

The query uses one repeatable-read snapshot, half-open current and previous
seven-day windows, a 15-second statement limit and a two-second lock limit. It
exports no identities, contact details, tokens, captions or raw analytics events.
The summarizer refuses missing periods, foreign fields, negative/boolean counts,
cutoff mismatches and exports collected before the cutoff. It preserves an
undefined percentage when the previous count is zero.

`recorded_signups` counts stored signup timestamps, not unique humans or all
historical accounts. Deleted records and missing timestamps remain a coverage
limitation. `recorded_first_values` counts persisted first-value milestones in
the window across signup cohorts; it is not a conversion rate for that week's
signups. `artist_profiles_all_origins` counts created profiles, including imported
or administrator-created profiles. No origin/claim classifier is inferred.
Professional profiles, human signups, imported/unclaimed profiles, weekly active
users and campaign conversions remain explicitly unavailable. A supplied export
is evidence provided by the operator, not an independently verified production
snapshot. Do not publish private inputs or substitute synthetic fixtures for
production results. The existing PostHog funnel contract remains unchanged.

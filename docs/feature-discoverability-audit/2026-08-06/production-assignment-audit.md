# Anonymized production assignment audit

The inspection was read-only. Queries returned aggregate counts and non-identifying role combinations only. No names, email addresses, tokens, party IDs, credential IDs, or record contents were retained in this packet.

## Health and deployment baseline

- Fly application: two started machines in separate regions.
- `/health`: application and database healthy at inspection time.
- `/version`: deployed source matched the then-current default-branch revision `ce0c3bc19e2d9030e871480e9e93790940c9eb12`.
- The feature-discoverability revision in this branch has not been deployed.

## Assignment aggregates

- 79 active credentials covering 73 active user parties.
- 8 inactive credentials.
- 5 parties have more than one active credential; this is an authentication-hygiene review item, not evidence of broader feature access by itself.
- 73 active parties have at least one active role; zero active parties were found without roles.
- 32 inactive historical role rows were retained.
- Exactly one verified active credential path has the coherent strict-administrator combination. This is below the desired emergency-access redundancy.
- Active role-row counts: A&R 2, Accounting 2, Admin 2, Artist 19, Artista 3, Customer 76, DJ 3, Engineer 3, Fan 74, Intern 3, Live Sessions Producer 1, Manager 2, Producer 3, Promoter 1, Promotor 1, ReadOnly 2, Reception 2, Student 17, Studio Manager 1, Teacher 6, Webmaster 1.
- Most active users have the baseline Customer + Fan roles. Other combinations exist for artists, students, interns, operational staff, producers/A&R, management, read-only staff, and one strict administrator.

Modules are deterministically derived from effective roles in backend code rather than stored as independent grants. The audit therefore found no separate production module-assignment table that could contradict roles. It did find legacy synonyms (`Artist`/`Artista`, `Promoter`/`Promotor`) and many zero-active role kinds; they are marked for review, not deletion.

Usage data was not used to infer privilege. Aggregate activity at inspection time contained 38 audit rows in the available period, primarily authentication events, no notifications, 138 locale preference rows, and no existing feature-discovery telemetry.

## Safety decision

No production correction was applied. With only one verified emergency administrator path, even a demonstrably tidy-looking role cleanup would create disproportionate lockout risk. Inactive roles, duplicate active credentials, and legacy role synonyms remain review items.

Because no production assignments or preferences were changed, there is no affected-record backup identifier. A backup/export is mandatory immediately before any future bounded correction. The required sequence is: encrypted authorization export, dry-run diff, verify at least two emergency administrator paths, run all role fixtures, apply one bounded batch, and re-query effective access before continuing.

## Historical credential risk

The source audit found repository-known development credential defaults in the native sign-in auto-fill, seed data, and operational scripts. This branch removes those values, requires runtime-only strong seed secrets, defaults write-capable scripts to localhost, and adds a regression proving the mobile fields start empty.

Source history cannot prove whether any current production password still matches a historical default, and the audit did not attempt logins or extract password hashes. Treat every credential or API token historically created by those seed paths as requiring rotation. The production-safe follow-up is to identify affected active records internally, export them to encrypted storage, verify a second emergency administrator, rotate one bounded account/token batch through the approved secret-delivery channel, confirm owner recovery, and only then revoke the previous values. No replacement credential may appear in chat, logs, screenshots, reports, or source control.

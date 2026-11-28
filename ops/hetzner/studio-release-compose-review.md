# Studio gallery release and the SMTP image override

## Observed production configuration, November 21, 2026

The public API reported commit `d4bb96030b1182e165ff39b614315c6a2554aee7`. On the production host, the API container listed both `/opt/tdf/production/compose.yaml` and `/opt/tdf-mail/api-smtp-image-override.yaml`. The canonical inventory collector rejected this at `container-api: compose-files`.

The two effective Compose models were rendered with all profiles enabled and compared in memory. The other services had identical configurations. The only differing API field was `image`. The base file specified `diegueins680/tdf-hq@sha256:7add54221fddcde47b89dd9c5f8328ea8ebb4f8166354f206de2f1fb2a120edc`. The override specified the corresponding bare image ID. Both resolved to the same local Docker image, which carried the base repository digest. Rendering the models requires access to secrets. Compare them in memory and report only the names of fields that differ.

## Proposed operator action

For the gallery rollout, use the guarded routine release in the [production runbook](README.md#guarded-routine-release). Its runtime observer explicitly supports the image-only override and records its hash. Its cutover and rollback update the override together with the base image reference. Keep the override in place for this rollout. The release coordinator's own checks must pass against the live host before fencing traffic.

If canonical inventory collection is also required, consolidate the Compose invocation in a separate approved maintenance window. Locate every service, timer and deployment script that supplies the second file. Check that the registered recovery image is retrievable and that the backup contains the base file, override and protected environment. Recompare both effective models and resolved image IDs immediately before the change. Any extra difference requires a new review.

With writers fenced and a tested recovery path available, recreate only the API from the base file with the exact running image. Retain the override for rollback. Confirm the API container identifies only the base file, the running image ID matches the recorded ID, the canonical collector succeeds, and public version and database health match the pre-change observations. Verify SMTP configuration parity and complete an authorized delivery probe to the designated test mailbox before reopening. If verification fails, restore the original two-file invocation and verify the previous API and SMTP behavior before reopening.

## Backup and staging evidence

Read-only SFTP using the installed release identity and pinned host key reached the release sub-account's `/home/releases` directory and found its qualification directory. Use `--storage-directory releases` relative to that sub-account's root. The historical local release journal ends with an observed reopen. The current release still needs a staging rehearsal of the selected candidate, backup retrieval, isolated restore, rollback and gallery smoke checks. Archive the receipts with the release.

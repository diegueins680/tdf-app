# Outbound worker admission

`NOTIF-WORKER-001` requires separate explicit startup configuration for automatic
social replies and daily course payment reminders. `SOCIAL_AUTO_REPLY_ENABLED`
and `COURSE_PAYMENT_REMINDER_ENABLED` default false; malformed values abort
configuration loading. Disabled workers do not create a thread or access their
queue, RAG service, messaging provider or SMTP transport. Credential presence
alone does not enable either worker. Each decision emits a content-free log.

The candidate must keep both disabled for the first upgrade. Enabling either is
not currently qualified: the existing algorithms persist success after an external
send and lack a durable pre-dispatch identity. Process death or a lost provider
reply can leave an eligible message and cause a later duplicate. No automatic
retry may be inferred to be safe from a missing local success record. Exceptions
used by these loops must preserve asynchronous cancellation.

The startup gate is a containment measure, not dispatch idempotency. An enabled
worker retains the existing behavior until a separately verified durable claim
and reconciliation path replaces it. Campaign automation already preserves an
unknown-delivery hold; that evidence must not be reset. Ticket confirmation has
a separate gate and remains disabled during first-upgrade qualification.

The currently running legacy image does not implement these two gates. Restoring
its unchanged configuration can restart outbound processing. A candidate-only
flag therefore cannot qualify original-image abort recovery. First shutdown and
abort require a separately verified outbound restriction or explicit reconciled
legacy policy. Neither live credential presence nor clean database recovery
establishes absence of external effects. No production setting is changed by this
component.

Tests exercise absent, independent true/false and malformed configuration, and
the actual disabled worker entrypoints. The latter use an unusable database and
assert disabled rather than scheduled logs. This does not model provider delivery,
interprocess replay, database concurrency, message privacy or transport retries;
those remain explicit repair obligations.

SMTP acceptance can precede loss of the sender's acknowledgement; see RFC5321
section4.5.3.2.6. We adopt conservative unknown-outcome handling as the target,
not exactly-once claims. PostgreSQL transaction-level advisory locks may serialize
future per-recipient claims, but every competing writer must honor them and no
lock makes the external provider effect transactional.

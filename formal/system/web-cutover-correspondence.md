# Web cutover correspondence

These mappings preserve the current reviewed cutover, privacy, inventory and public Mobile disclosure decisions from #468, #485 and #486. Canonical runtime observation and retired release guards are mapped separately by DEPLOY-OBSERVE-001 and DEPLOY-TARGET-001. Implementation paths are not blanket coverage: each requirement states its boundary, and tests prove only their exercised cases. Public disclosure wording has source review, not an invented automated test.

Existing source and unit tests do not establish production Google sign-in/upload, provider telemetry receipt, official payment qualification, store admission or complete guarded backend deployment. Each requires its own actual evidence. Inventory pagination is sequential and not a transactional snapshot; uploads are not exactly once. Messaging checks do not authorize credential rotation. Read-only mail monitoring sends no messages.

The reviewed ticket confirmation contract remains formal/ticket-admission/confirmation-delivery.md: SMTP acceptance is not inbox delivery; retries can duplicate messages, never authorize a second charge. The worker remains disabled until explicitly qualified and enabled. No migration or feature activation follows merely from this mapping.

## Event presentation requirement identity

`EVT-PRESENTATION-001` is the canonical combined event/transaction metadata requirement. It incorporates the earlier `EVT-PUBLIC-001` scope from ce4222633, preserving its implementation paths, regression tests and privacy obligations. The requirement register records that predecessor; source ancestry retains the complete original declaration. This identifier reconciliation adds no runtime guarantee.

## Canonical public links and read-only social diagnostics

`OPS-CLIENT-001` also covers Config's default public web origin `https://www.tdfrecords.net` and asset origin `https://api.tdfrecords.net/assets/serve`. Validated explicit deployment overrides remain authoritative. The configuration regression exercises absent overrides and retains the existing explicit-override test; the directory fixture checks rendered image URLs against the canonical API. These checks do not prove production configuration.

The social diagnostic requires an explicit versioned Meta Graph origin, uses only bounded reads with redirects rejected, reports unhealthy or malformed results as failures, and redacts configured credentials including backend-supported aliases and URL-encoded forms. Synthetic transport tests exercise those boundaries without sending messages, changing subscriptions or restarting services. Removed printed repair commands are not the diagnostic contract. Provider repair and live delivery qualification remain separate authorized operations. The native-broadcast quarantine merged in #478 is retained; historical hostname changes in removed unsafe routines do not authorize restoring them.

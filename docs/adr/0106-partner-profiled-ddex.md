# ADR-0106: Partner-profiled DDEX and evidence-based status

Status: Accepted — 2026-08-13

## Decision

Every delivery pins an immutable release version, recipient/profile version, ERN schema/profile,
transport, terms, message ID, manifest, assets, and checksums. Delivery state advances only on stored
transport and recipient evidence. Production delivery is disabled without a contracted partner,
implementation licence/DPID arrangement, credentials, conformance fixtures, and explicit approval.

A global hard-coded DDEX version, generated XML as "delivered," and mocked acknowledgement as
production evidence were rejected. ERN 4.3.2 is the initial internal baseline; recipient requirements
may pin a different supported profile through the registry.

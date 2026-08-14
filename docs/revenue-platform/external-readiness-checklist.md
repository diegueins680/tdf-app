# External dependency and production-approval checklist

Unchecked items are blockers, not optional polish.

## Merchant and payments

- [ ] Datafast sandbox/live application, merchant/entity/MID/TID and test evidence.
- [ ] Datafast notification authentication, 3-D Secure, refund/void/partial-refund, installment,
      tokenization/recurrence and reconciliation capabilities confirmed in writing.
- [ ] Datafast production certification and separately authorized low-value verification window.
- [ ] PayPal sandbox/live apps and webhook IDs stored in the secret manager.
- [ ] PayPal webhook signature/replay suite and create/capture/refund reconciliation pass.
- [ ] PayPal Subscriptions and/or Payouts approval, if those distinct products are desired.
- [ ] Merchant-of-record, consumer, Ecuador/launch-market tax and electronic-invoice review.
- [ ] Refund, dispute, chargeback, manual evidence and separation-of-duties policy approved.

## Assets, privacy, and security

- [ ] Private object store, encryption, ACL, signed-link, retention/deletion, quarantine, backup and
      restore ownership configured.
- [ ] Malware scanner and audio/artwork probing service configured and tested.
- [ ] Secret rotation, redacted logging, least privilege, edge rate limiting and abuse response tested.
- [ ] Ecuador and launch-jurisdiction privacy/data-retention review completed.

## Distribution and rights

- [ ] DDEX Implementation Licence/DPID or partner-issued identity model confirmed.
- [ ] Contracted distribution partner, store/territory coverage and responsibility matrix signed.
- [ ] Partner ERN/profile/rules, transport, credentials, acknowledgement/rejection/live semantics,
      correction/takedown and DSR/report formats verified in sandbox.
- [ ] ISRC registrant and UPC/EAN/GRid assignment provenance documented.
- [ ] Distribution agreement, rights warranty, content/refund/privacy policies reviewed.
- [ ] Workflows reviewed for covers, samples, impersonation, AI material, explicit content, fraud,
      sanctions, minors, complaints, counter-notices and repeat infringement.
- [ ] Emergency takedown and support escalation ownership rehearsed.

## Royalties and settlement

- [ ] Partner settlement/report controls reconcile on licensed fixtures and a contracted sandbox.
- [ ] Currency/FX, deductions, TDF royalty share, reserves, recoupment, rounding, corrections,
      disputes, negative balances and statement terms approved.
- [ ] Beneficiary KYC/tax/bank verification and payout-account change controls approved.
- [ ] Payout cadence, threshold, reserve period and dispute window approved.
- [ ] Manual settlement dual-control pilot reconciles; automatic payout remains disabled.

## Exact separate production authorizations

The following each require an explicit authorization naming environment, scope, limit and time
window: real charge/capture, real refund/void, real chargeback/dispute action, live merchant flag,
real DSP/partner delivery, real correction, real takedown, real statement issuance, movement of
royalty or third-party funds, and automatic payout enablement.

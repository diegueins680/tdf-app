# Distribution standards, partner assumptions, and pricing review

Research date: 2026-08-13. Only official standards/provider pages are used. Public prices can change;
the database seeds are review inputs, not approved offers.

## Standards baseline

- DDEX currently recommends ERN 4.3.2; the standards catalogue lists ERN schema 4.3.2 and release
  profiles 2.3.1. The initial internal parser detects conventional audio single/EP/album ERN 4.3.2.
- The catalogue lists DSR Part 1 and Part 3 version 1.4, Part 8 record types 1.5.1, and Part 9
  financial reporting 1.2. These are design targets; no DSR runtime parser is claimed here.
- RIN 2.1 and MEAD 1.1 are future adapter families, not implemented capabilities.
- Development/evaluation can occur under the evaluation terms, but commercial message exchange
  requires a free DDEX Implementation Licence. A DPID identifies message senders and recipients.

Primary sources:

- <https://kb.ddex.net/implementing-each-standard/electronic-release-notification-message-suite-%28ern%29/>
- <https://kb.ddex.net/reference-material/standards-specifications/>
- <https://kb.ddex.net/general-implementation-guidance/licensing-the-standards/>
- <https://kb.ddex.net/general-implementation-guidance/licensing-the-standards/ddex-party-identifier-%28dpid%29/>

No recipient profile is assumed. Each contracted partner must pin its ERN/release profile,
business rules, DPID pair, transport, credentials reference, acknowledgement/rejection semantics,
live evidence, correction/takedown protocol, usage-report format, and test/production boundaries in
the versioned partner registry.

## Official public market benchmarks

| Product | Public price observed | Revenue model |
|---|---:|---|
| CD Baby single | USD 9.99 one-time | 9% of download/streaming revenue |
| CD Baby album | USD 14.99 one-time | 9% of download/streaming revenue |
| TuneCore Rising Artist | USD 24.99/year | Annual subscription; social monetization has a separate 20% fee |
| TuneCore Breakout Artist | USD 44.99/year | Annual subscription |
| TuneCore Professional | USD 54.99/year | Annual subscription |
| DistroKid Musician | USD 24.99/year | Annual, unlimited releases, advertised 100% earnings; extras separate |
| DistroKid Musician Plus | USD 44.99/year | Annual |
| DistroKid Ultimate | USD 89.99/year | Annual |

Sources:

- <https://support.cdbaby.com/hc/en-us/articles/213125406-How-much-does-CD-Baby-cost>
- <https://www.tunecore.com/pricing>
- <https://distrokid.com/product/distrokid/plans-and-pricing-2/>
- <https://support.distrokid.com/hc/en-us/articles/360013648973-How-Much-Does-DistroKid-Cost>

## Recommendation

The repository strategy's provisional USD 50–200 per-release range is materially above commodity
self-service distribution. It is defensible only as a curated Ecuador/Latin America label-service
offer with explicit staff QC, metadata/rights assistance, partner handling, bilingual support,
defined revisions/SLA, and transparent royalty share—not as bare DSP delivery.

The inactive seed version proposes USD 50 single, USD 100 EP, USD 150 album, USD 60 annual catalog
management, USD 25 monthly artist-domain product with automatic renewal disabled, USD 30 priority
review, USD 50 metadata/QC assistance, and a provisional 10% royalty share on core products. The
price, taxes, scope, support load, partner costs, refund exposure, royalty share, and margin must be
reviewed before activation. Approval creates an immutable version; changes require a new version.

## Required commercial evidence

- Named distribution partner contract and territory/store coverage.
- DDEX licence/DPID allocation model and ISRC/UPC/EAN provenance.
- Per-format labor/cost model, tax/invoicing advice, refund policy, delivery SLA, support scope, and
  competitor recheck.
- Royalty percentage mechanics, partner deductions, reserve/recoupment policy, payout threshold,
  currency/FX policy, KYC/tax/legal gates, and dual-approval settlement procedure.
- Pilot results for validation time, rejection rate, delivery/acknowledgement latency, support time,
  release-date misses, report variance, and statement disputes.

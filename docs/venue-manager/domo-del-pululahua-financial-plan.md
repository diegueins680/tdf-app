# Domo del Pululahua financial project draft

Prepared for the `venue-manager` OpenClaw organization.

Last updated: 2026-04-18

## Public facts verified

- Domo del Pululahua presents itself as an event space on the edge of the Pululahua crater near Quito.
- Public positioning includes weddings, corporate events, concerts, workshops, spiritual retreats, and photo sessions.
- The current public Domo website has one visible blog post and a contact form, but no published capacity, rate card, package terms, booking policy, or financing deck.
- Pululahua is north of Quito in Pichincha province. Public volcanology references describe the Pululahua complex as a depression with lava domes and as potentially active.
- Nearby event competitor El Crater advertises workshops, seminars, weddings, baptisms, birthdays, group celebrations, lodging support, catering, and outdoor capacity up to 200 people.
- Banco Central del Ecuador's April 2026 maximum effective annual rates list Productivo PYMES at 10.28%, Productivo Empresarial at 11.00%, and Consumo at 16.77%. This supports positioning the request as productive business financing rather than consumer debt.

Sources:

- https://www.domopululahua.com/
- https://www.elcrater.com/en/eventos
- https://www.volcanesdelecuador.com/pululahua
- https://contenido.bce.fin.ec/documentos/Estadisticas/SectorMonFin/TasasInteres/Indice.htm

Related lender packet documents:

- `docs/venue-manager/domo-lender-one-pager.md`
- `docs/venue-manager/domo-lender-one-pager-print.html`
- `docs/venue-manager/domo-36-month-pro-forma.md`
- `docs/venue-manager/domo-36-month-pro-forma.csv`
- `docs/venue-manager/domo-lender-outreach-table.md`
- `docs/venue-manager/domo-loan-document-checklist.md`
- `docs/venue-manager/domo-loan-packet/`

## Owner supplied assumptions

- Target loan request: USD 100,000.
- Desired loan term: longest available.
- Maximum comfortable monthly payment: USD 1,000.
- The property is owner owned.
- Borrower options: TDF Records or the owner personally, depending on lender probability.
- Venue shell is substantially constructed.
- Remaining funded work: landscaping, part of the road, furniture, signage, and working capital.
- Permits are in process.
- Domo will operate catering, bar, sound, and lights directly.
- Domo will broker the rest through third-party vendors.
- Confirmed size basis: 16 m diameter, which gives about 201 m2 of gross circular floor area.
- First lender targets: banks and cooperatives.

## Loan objective

Launch Domo del Pululahua as a bookable destination venue with a professional quote and reservation funnel, operational readiness, and enough working capital to convert private events into predictable cash flow.

Requested financing: USD 100,000.

Payment constraint: the structure should keep debt service at or below USD 1,000 per month. At the April 2026 BCE Productivo PYMES ceiling of 10.28% effective annual interest, that requires roughly an 18 year amortization. If lenders only offer 10 to 15 years, the request must either be reduced, supported by a grace period, structured with a balloon/residual payment, or supplemented with owner equity.

Working target: ask for an 18 to 20 year productive/business facility, with the owner property as collateral if needed, and with the right to prepay once event cash flow stabilizes.

Primary channels: banks and cooperatives. The first outreach should compare at least three offers on rate, term, collateral requirement, grace period, prepayment rules, appraisal treatment, and whether the request can be underwritten as productive SME credit instead of personal consumer credit.

## Borrower strategy

Best first approach: apply through TDF Records as the operating company, with the owner as guarantor and the property offered as collateral if the lender requires it.

Reasoning:

- The use of funds is productive: venue launch, infrastructure completion, furniture, signage, operating equipment, and working capital.
- Productive/business credit is cheaper than consumer credit under current BCE ceilings.
- Company borrowing keeps venue revenue, expenses, taxes, deposits, and lender reporting in one business ledger.
- The owner owned property can still strengthen the application through collateral or a personal guarantee.

Fallback: apply personally only if the company does not yet have enough financial history or if a specific lender can offer materially better approval odds, rate, term, or collateral treatment. A personal loan should still document that proceeds are invested into TDF Records or the Domo venue operation so the accounting trail is clear.

## Business model

Primary revenue:

- Venue rental for weddings and private celebrations.
- Corporate offsites, workshops, and brand activations.
- Retreats and wellness programming.
- Concerts, intimate performances, and cultural events.
- Photo, video, and content production sessions.

Secondary revenue:

- Production coordination fee.
- Catering and bar packages operated by Domo.
- Sound and lighting packages operated by Domo.
- Transport coordination margin.
- Preferred vendor commissions where legally and commercially appropriate.
- Overnight or partner lodging package margin if lodging is added later.

## First public funnel

Implemented target page:

- Canonical route: `https://tdf-app.pages.dev/domo-del-pululahua`
- Alternate route redirects: `https://tdf-app.pages.dev/venues/domo-del-pululahua`
- API used for booking intake: `POST https://tdf-hq.fly.dev/bookings/public`
- Quote logic is currently client-side so the public page works against the deployed API without requiring a backend release.

The quote tool currently estimates:

- Event base price by event type.
- Venue use by hours.
- Mounting and teardown hours.
- Extra guest charges above included guests.
- Optional catering/bar operation.
- Optional sound and lighting operation.
- Optional Quito to Pululahua transport coordination.
- IVA at 12%.
- Suggested 40% deposit to separate a date.

These numbers are planning defaults, not a final published rate card.

## Startup use of funds

Working loan allocation for lender discussion:

| Category | Base budget | Notes |
| --- | ---: | --- |
| Landscaping and site finish | USD 22,000 | Paths, exterior areas, guest flow, photo spots, drainage, weather resilience. |
| Road and access completion | USD 18,000 | Final road work, parking flow, arrival safety, basic wayfinding. |
| Event furniture and operating equipment | USD 20,000 | Tables, chairs, linens, storage, service equipment, heaters or covers as needed. |
| Signage, launch content, and sales collateral | USD 7,000 | Road signage, venue signage, photo/video, lender deck, quote collateral. |
| Catering and bar launch kit | USD 10,000 | Service stations, bar equipment, inventory start, food safety setup. |
| Sound and lighting operating base | USD 8,000 | Core package for speeches, background music, ceremonies, small performances. |
| Permits, legal, insurance, engineering | USD 7,000 | Permit processing, contracts, inspection support, civil liability coverage. |
| Working capital reserve | USD 13,000 | Staff, marketing, utilities, maintenance, deposits, and vendor float. |
| Contingency | USD 5,000 | Weather, price increases, road surprises, maintenance. |
| Total loan request | USD 100,000 | Matches requested principal. |

## Debt service fit

Payment estimates for USD 100,000 at 10.28% effective annual interest:

| Term | Estimated monthly payment | Fit vs USD 1,000 target |
| --- | ---: | --- |
| 10 years | USD 1,312 | Too high |
| 12 years | USD 1,185 | Too high |
| 15 years | USD 1,064 | Slightly high |
| 18 years | USD 989 | Fits |
| 20 years | USD 953 | Fits |

If a lender caps the term below 18 years at a similar rate, keep the USD 1,000 payment ceiling by reducing principal to approximately:

| Term | Principal supported by USD 1,000/month |
| --- | ---: |
| 10 years | USD 76,000 |
| 12 years | USD 84,000 |
| 15 years | USD 94,000 |

## Unit economics assumptions

Planning assumptions to validate:

- Planning gross indoor area: about 201 m2, based on the confirmed 16 m diameter.
- Practical event capacity before final permits: 70 to 100 guests for seated premium events; 60 to 90 for workshops or retreats; 120 to 160 for cocktail, standing, or concert layouts if exits, bathrooms, parking, and safety plan support it.
- Average booked event duration: 6 to 8 hours plus 1 to 3 hours setup.
- Average venue revenue per event: USD 1,600 to USD 3,400 before optional services.
- Average gross margin on venue rental: 55% to 70% after cleaning, staffing, utilities, coordination, and maintenance.
- Average gross margin on direct catering/bar/sound/lights: 30% to 55%, depending on staffing, inventory waste, rental needs, and supplier pricing.
- Average gross margin on brokered vendor services: 10% to 25%, depending on referral or coordination agreements.

## 12 month revenue scenarios

| Scenario | Events/month | Average invoice | Contribution margin | Monthly contribution before fixed costs |
| --- | ---: | ---: | ---: | ---: |
| Conservative | 2 | USD 3,500 | 45% | USD 3,150 |
| Base | 4 | USD 5,000 | 50% | USD 10,000 |
| Growth | 6 | USD 6,500 | 52% | USD 20,280 |

Base case break-even target: with USD 4,000 of lean fixed cost and USD 1,000 of debt service, the venue needs about USD 5,000 of monthly contribution. That is roughly two strong events per month at USD 5,000 average invoice and 50% contribution margin.

## Fixed operating cost assumptions

Lean monthly cost target:

- Venue lead and booking coordination: USD 900 to USD 1,400.
- Cleaning, maintenance, and grounds: USD 500 to USD 900.
- Utilities, internet, security, and waste: USD 350 to USD 700.
- Insurance, accounting, legal retainers: USD 300 to USD 650.
- Marketing and content: USD 600 to USD 1,200.
- Software, payment, CRM, hosting: USD 120 to USD 300.

Target fixed cost range: USD 2,770 to USD 5,150 per month before debt service.

Debt service target: no more than USD 1,000 per month.

## Lender package outline

The final application packet should include:

- Executive summary with Domo positioning and target customer segments.
- Verified site capacity and permitted use.
- Owner property documentation and any collateral appraisal available.
- TDF Records corporate documents, tax history, bank statements, and owner guarantee package if applying through the company.
- Capex quotes and contractor/vendor quotes.
- 12, 24, and 36 month pro forma.
- Cash flow schedule with loan payment coverage.
- Booking pipeline evidence: inquiries, letters of intent, launch partnerships.
- Rate card and contract/deposit policy.
- Risk plan for weather, access, noise, safety, cancellation, and volcano/geological disclosures.

## Immediate next work

- Confirm the final public route and deploy the new quote/booking page.
- Replace planning rates with approved Domo rate card.
- Seed Domo venue metadata in the protected Social Events venue catalog when an admin token is available.
- Add a staff workflow to convert public booking requests into holds, invoices, deposits, and confirmed events.
- Add a financing dashboard that tracks inquiry count, quote count, deposits, booked revenue, and debt service coverage.

## Questions needed before submitting a loan packet

1. Does TDF Records already have financial statements, tax filings, and bank statements strong enough for underwriting?
2. Is the owner willing to mortgage or pledge the Domo property, or should the request avoid property collateral if possible?
3. What is the latest expected date for permits needed for events, alcohol, amplified music, food service, and late-night operations?
4. What is the target first paid event date and how many leads or soft commitments already exist?
5. Should quote prices stay public estimates or become private-by-request after enough leads arrive?

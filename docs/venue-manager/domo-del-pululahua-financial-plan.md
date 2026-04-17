# Domo del Pululahua financial project draft

Prepared for the `venue-manager` OpenClaw organization.

Last updated: 2026-04-17

## Public facts verified

- Domo del Pululahua presents itself as an event space on the edge of the Pululahua crater near Quito.
- Public positioning includes weddings, corporate events, concerts, workshops, spiritual retreats, and photo sessions.
- The current public Domo website has one visible blog post and a contact form, but no published capacity, rate card, package terms, booking policy, or financing deck.
- Pululahua is north of Quito in Pichincha province. Public volcanology references describe the Pululahua complex as a depression with lava domes and as potentially active.
- Nearby event competitor El Crater advertises workshops, seminars, weddings, baptisms, birthdays, group celebrations, lodging support, catering, and outdoor capacity up to 200 people.

Sources:

- https://www.domopululahua.com/
- https://www.elcrater.com/en/eventos
- https://www.volcanesdelecuador.com/pululahua

## Loan objective

Launch Domo del Pululahua as a bookable destination venue with a professional quote and reservation funnel, operational readiness, and enough working capital to convert private events into predictable cash flow.

Initial loan request should be sized after confirming property, capacity, permits, and capex quotes. A first working range is USD 80,000 to USD 140,000.

## Business model

Primary revenue:

- Venue rental for weddings and private celebrations.
- Corporate offsites, workshops, and brand activations.
- Retreats and wellness programming.
- Concerts, intimate performances, and cultural events.
- Photo, video, and content production sessions.

Secondary revenue:

- Production coordination fee.
- Catering and bar coordination margin.
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
- Optional catering/bar coordination.
- Optional technical production.
- Optional Quito to Pululahua transport coordination.
- IVA at 12%.
- Suggested 40% deposit to separate a date.

These numbers are planning defaults, not a final published rate card.

## Startup use of funds

Working loan allocation for lender discussion:

| Category | Base budget | Notes |
| --- | ---: | --- |
| Safety, permits, legal, municipal setup | USD 8,000 | Confirm land use, event permits, civil liability, contracts. |
| Venue readiness and basic infrastructure | USD 28,000 | Bathrooms, power, water, signage, access, parking, waste plan. |
| Event furniture and operating equipment | USD 18,000 | Tables, chairs, tents or covers, heaters, linens, storage. |
| Sound, lighting, and production base | USD 16,000 | Minimum viable production package for speeches and small events. |
| Kitchen, bar, and vendor service area | USD 12,000 | Basic prep/service area if not outsourcing fully. |
| Marketing, website, booking, photo/video | USD 9,000 | Launch campaign, imagery, CRM, quote flow, collateral. |
| Working capital reserve | USD 24,000 | 6 months of lean fixed costs and deposits to vendors. |
| Contingency | USD 10,000 | Weather, access, price increases, maintenance. |
| Total working need | USD 125,000 | Adjust after quotes and engineering review. |

## Unit economics assumptions

Planning assumptions to validate:

- Practical event capacity: 80 to 120 guests for premium events until site measurements and safety review are complete.
- Average booked event duration: 6 to 8 hours plus 1 to 3 hours setup.
- Average venue revenue per event: USD 1,600 to USD 3,400 before optional services.
- Average gross margin on venue rental: 55% to 70% after cleaning, staffing, utilities, coordination, and maintenance.
- Average gross margin on add-ons: 15% to 30%, depending on whether Domo operates or brokers the service.

## 12 month revenue scenarios

| Scenario | Events/year | Avg venue revenue | Add-on gross margin | Gross profit estimate |
| --- | ---: | ---: | ---: | ---: |
| Conservative | 36 | USD 1,750 | USD 15,000 | USD 55,950 |
| Base | 60 | USD 2,250 | USD 32,000 | USD 119,750 |
| Growth | 84 | USD 2,800 | USD 55,000 | USD 219,640 |

Gross profit estimate assumes 65% gross margin on venue rental plus the listed add-on gross margin. This is directional until operating cost quotes are confirmed.

## Fixed operating cost assumptions

Lean monthly cost target:

- Venue lead and booking coordination: USD 900 to USD 1,400.
- Cleaning, maintenance, and grounds: USD 500 to USD 900.
- Utilities, internet, security, and waste: USD 350 to USD 700.
- Insurance, accounting, legal retainers: USD 300 to USD 650.
- Marketing and content: USD 600 to USD 1,200.
- Software, payment, CRM, hosting: USD 120 to USD 300.

Target fixed cost range: USD 2,770 to USD 5,150 per month before debt service.

## Lender package outline

The final application packet should include:

- Executive summary with Domo positioning and target customer segments.
- Verified site capacity and permitted use.
- Owner or lease documentation.
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

1. What is the exact loan amount being requested, expected term, target bank/cooperative, and likely interest rate?
2. Does Domo own the property, lease it, or operate under another agreement?
3. What is the legally permitted event capacity, parking capacity, and latest fire/safety inspection status?
4. Which services will Domo operate directly versus broker through vendors?
5. What capex already exists and what still needs to be purchased before the first paid event?
6. What is the target first paid event date and how many leads or soft commitments already exist?
7. Are alcohol, amplified music, lodging, camping, and late-night events allowed at the site?
8. Should quote prices be public, private-by-request, or shown only as estimates?

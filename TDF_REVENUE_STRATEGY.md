# TDF Revenue Strategy & Implementation Report

**Date:** August 4, 2026  
**Scope:** Full product audit, market research, opportunity scoring, implementation plan

---

## 1. Executive Summary

TDF Records has a mature backend infrastructure (Haskell/Servant API, React UI, Expo mobile) with CRM, scheduling, invoicing, inventory, and a service catalog — but **no public-facing revenue-generating storefront** for its core music services. The existing marketplace sells equipment rentals/sales (low margin, limited market), while the highest-value services (recording, mixing, mastering, classes) are managed internally via admin tools only.

**The single biggest revenue opportunity is launching a public Remote Mixing & Mastering storefront** — a vertical slice that lets artists worldwide discover, purchase, and receive professional mixing/mastering services with online checkout. This leverages existing infrastructure (pipelines, Kanban, service catalog, Stripe/PayPal code) and addresses a proven global market (SoundBetter, AirGigs, Fiverr) with almost zero competition in Ecuador/LATAM.

**Critical payment finding:** Stripe is **NOT available in Ecuador**. Only Brazil and Mexico are supported in LATAM. The codebase references Stripe extensively, but production deployment for Ecuadorian merchants requires Datafast (local cards) + PayPal (international) as primary payment rails. PayPhone is a viable fallback (5% + IVA).

**Recommended immediate action:** Implement the Remote Mixing & Mastering storefront as a complete vertical slice with Datafast + PayPal checkout, targeting first revenue within 2 weeks of deployment.

---

## 2. Current-State Product & Repository Audit

### Architecture
| Component | Stack | Status |
|-----------|-------|--------|
| Backend API | Haskell + Servant + PostgreSQL 16 + Persistent | ✅ Mature |
| Web UI | React + Vite + MUI + React Query + TypeScript | ✅ Mature |
| Mobile App | Expo + React Native + React Query | ✅ Functional |
| Deployment | Fly.io (backend) + Cloudflare Pages (frontend) | ✅ Configured |

### What's Complete
- **CRM & party management** — unified party model, roles, WhatsApp/Instagram integration
- **Resource scheduling** — multi-resource booking (studios, rooms, classrooms)
- **Service catalog** — 9 services (Recording, Mixing, Mastering, Rehearsal, Classes, Event Production, Podcast, DJ Booth)
- **Pipeline/Kanban** — Mixing and Mastering workflow stages
- **Invoicing & payments** — multi-method (Cash, BankTransfer, CardPOS, PayPal, Stripe, Wompi, PayPhone, Crypto)
- **Package management** — hour-based packages with expiration tracking
- **Inventory & equipment** — serial tracking, QR codes, maintenance scheduling
- **Artist profiles & fan clubs** — public pages, Spotify/YouTube integration
- **Event discovery & ticketing** — external event imports, Stripe Checkout
- **DDEX catalog infrastructure** — schema for releases, resources, identifiers, credits, deals (added Aug 2, 2026)
- **Course registration** — with Stripe PaymentIntent and subscription support
- **Artist tipping** — Stripe Connect destination charges with platform fee

### What's Missing or Broken
1. **No public service storefront** — services are only bookable via admin UI
2. **No self-service checkout for recording/mixing/mastering** — requires manual invoicing
3. **Stripe is non-functional in Ecuador** — the primary payment rail in the code cannot process Ecuadorian transactions
4. **Marketplace is equipment-only** — `MarketplaceListing` is tied to `Asset` (inventory), not services
5. **No provider onboarding** — no way for external engineers/producers to offer services
6. **No public API for service discovery** — no search, filtering, or service detail pages
7. **DDEX implementation is schema-only** — no API endpoints, no UI, no processing pipeline
8. **No analytics dashboard for revenue** — dashboard exists but doesn't track marketplace KPIs
9. **No review/rating system** for service providers
10. **No subscription/recurring revenue model** for ongoing services

### Current Customer Journey (As-Is)
```
Artist discovers TDF → Contacts via WhatsApp/Instagram → 
Manual quote → Admin creates invoice → Cash/bank transfer payment → 
Service delivered → Manual follow-up
```

### Target Customer Journey (To-Be)
```
Artist discovers TDF online → Browses services & pricing → 
Selects package → Pays online (Datafast/PayPal) → 
Uploads tracks → Engineer delivers via pipeline → 
Automatic notification → Review & reorder
```

---

## 3. Market & Competitor Findings

### Payment Provider Availability in Ecuador

| Provider | Available | Fees | Notes |
|----------|-----------|------|-------|
| **Stripe** | ❌ No | — | Only Brazil & Mexico in LATAM |
| **Stripe Connect** | ❌ No | — | Cross-border payouts not supported for Ecuador |
| **PayPal** | ✅ Yes | 3.4% + $0.30 (sending); 0.50% withdrawal (min $10) | Available for buyers and sellers |
| **Datafast** | ✅ Yes | Not published (calculator available) | Local card processing, all Ecuadorian cards |
| **PayPhone** | ✅ Yes | 5% + IVA (~5.6% total) | Card payments via TAP, links, QR |
| **Wompi** | ❌ No | — | Only Colombia, Panama, El Salvador |
| **Crypto** | ✅ Possible | Varies | Already in payment method enum |

**Implication:** The codebase's heavy Stripe dependency must be supplemented with Datafast (for local card payments) and PayPal (for international). The existing `DatafastCheckoutDTO` and `PaypalCreateDTO` types suggest these were already planned.

### Music Industry Market Data

**Global:**
- SoundBetter (acquired by Spotify) dominates the remote music services marketplace
- AirGigs charges 8-15% seller commission, no buyer fees
- Fiverr takes 20% from sellers
- The global music production services market is growing at ~8% CAGR

**Latin America:**
- Latin music streaming grew 30%+ YoY (IFPI 2024)
- Ecuador has a growing independent artist scene but almost no professional mixing/mastering marketplaces
- Most Ecuadorian artists send tracks to engineers in Mexico, Colombia, or the US
- Payment friction is the #1 barrier for LATAM artists buying services online

**Competitor Analysis:**

| Platform | Commission | Ecuador Focus | Payment Methods | Weakness |
|----------|-----------|---------------|-----------------|----------|
| SoundBetter | 15% | ❌ None | Stripe (US/EU only) | No LATAM payment support |
| AirGigs | 8-15% | ❌ None | PayPal, Stripe | No Spanish support, no local payments |
| Fiverr | 20% | ❌ Generic | PayPal, cards | Race to bottom on pricing |
| Local engineers | 0% | ✅ Direct | WhatsApp negotiation | No discovery, no trust signals, no escrow |

**TDF's competitive advantage:** Local presence in Ecuador + existing studio infrastructure + professional engineers + multi-language support + local payment methods (Datafast) + international payments (PayPal).

---

## 4. Service Opportunity Analysis

### Opportunity 1: Remote Mixing & Mastering Storefront
- **Target buyer:** Independent artists, bands, producers worldwide
- **Service provider:** TDF engineers (initially), then external providers
- **Customer problem:** Finding affordable, quality mixing/mastering with reliable delivery
- **Demand evidence:** SoundBetter ($$$ valuation by Spotify), AirGigs growth, Fiverr music category
- **Supply gap:** Zero dedicated mixing/mastering marketplaces in Ecuador; LATAM artists overpay for international services
- **Geographic opportunity:** Ecuador (local), LATAM (Spanish-speaking), then global
- **Pricing:** $80-200/mix, $40-100/master (competitive with international rates)
- **TDF commission:** 100% initially (TDF employs engineers); 20-30% when external providers join
- **Gross margin:** 70-80% (engineer salary vs. service price)
- **Time to revenue:** 1-2 weeks (infrastructure exists)
- **Scalability:** Unlimited (digital delivery, no geographic constraints)

### Opportunity 2: DDEX Distribution Gateway
- **Target buyer:** Record labels, independent artists with completed recordings
- **Service provider:** TDF as DDEX gateway operator
- **Customer problem:** Getting music onto Spotify/Apple Music from Ecuador is extremely difficult
- **Demand evidence:** 30%+ streaming growth in LATAM; DDEX schema already built
- **Supply gap:** Almost no DDEX-compliant distribution services in Ecuador
- **Pricing:** $50-200 per release + annual catalog fees
- **Commission:** 100% of upfront fee + revenue share on streaming
- **Time to revenue:** 2-3 months (needs DSP partnerships)
- **Scalability:** High but requires partnerships

### Opportunity 3: Studio Booking Marketplace
- **Target buyer:** Artists, bands, podcasters
- **Service provider:** TDF + partner studios
- **Customer problem:** Finding and booking quality studios online
- **Demand evidence:** Existing scheduling infrastructure, local demand
- **Supply gap:** No online studio booking in Ecuador
- **Pricing:** $25-80/hour depending on studio
- **Commission:** 15-20% for partner studios
- **Time to revenue:** 3-4 weeks
- **Scalability:** Limited by physical locations

### Opportunity 4: Online Music Lesson Marketplace
- **Target buyer:** Music students worldwide
- **Service provider:** TDF teachers + external instructors
- **Customer problem:** Access to quality music education, especially in Spanish
- **Demand evidence:** Existing class infrastructure, package system
- **Supply gap:** Few structured online music lesson platforms in Spanish
- **Pricing:** $20-50/hour
- **Commission:** 20-30% for external teachers
- **Time to revenue:** 2-3 weeks
- **Scalability:** High (digital delivery)

### Opportunity 5: Session Musician Marketplace
- **Target buyer:** Producers, artists needing specific instruments
- **Service provider:** Professional musicians
- **Customer problem:** Finding reliable session musicians remotely
- **Demand evidence:** AirGigs top categories include drummers, guitarists, bass players
- **Supply gap:** No dedicated LATAM session musician marketplace
- **Pricing:** $50-300 per track
- **Commission:** 15-20%
- **Time to revenue:** 4-6 weeks
- **Scalability:** High (digital delivery)

### Opportunity 6: Equipment Rental/Sales (Current Marketplace)
- **Target buyer:** Local musicians, studios
- **Service provider:** TDF inventory
- **Customer problem:** Access to quality gear without full purchase
- **Demand evidence:** Already built, some traction
- **Supply gap:** Limited local competition
- **Pricing:** Variable (rental/sale)
- **Commission:** 100% (TDF owns inventory)
- **Time to revenue:** Already available
- **Scalability:** Limited by inventory and geography

### Opportunity 7: Event Production Services
- **Target buyer:** Venues, festivals, corporate events
- **Service provider:** TDF production team
- **Customer problem:** End-to-end event production
- **Demand evidence:** Existing pipeline, local demand
- **Supply gap:** Few professional event production companies in Ecuador
- **Pricing:** $500-10,000+ per event
- **Commission:** 100% (TDF delivers)
- **Time to revenue:** 3-4 weeks (needs checkout)
- **Scalability:** Limited by team capacity

---

## 5. Weighted Scoring & Prioritized Roadmap

| Criterion | Weight | M&M Storefront | DDEX Gateway | Studio Booking | Online Lessons | Session Musicians | Equipment | Event Prod. |
|-----------|-------:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| Near-term revenue & margin | 20 | **18** | 12 | 10 | 12 | 10 | 8 | 12 |
| Speed to first revenue | 20 | **18** | 8 | 12 | 14 | 10 | 16 | 10 |
| Evidence of demand | 15 | **14** | 12 | 10 | 11 | 12 | 8 | 10 |
| Limited/fragmented supply | 10 | **9** | 9 | 8 | 7 | 8 | 6 | 7 |
| Fit for Ecuador/LATAM | 10 | **9** | 8 | 9 | 8 | 7 | 8 | 9 |
| User value & problem severity | 10 | **9** | 8 | 7 | 8 | 8 | 6 | 7 |
| Implementation feasibility | 10 | **9** | 5 | 8 | 8 | 7 | 9 | 7 |
| International scalability | 5 | **5** | 4 | 2 | 5 | 5 | 2 | 2 |
| **TOTAL** | **100** | **91** | **66** | **66** | **73** | **67** | **61** | **64** |

### Roadmap Groups

**🔴 Immediate Revenue (Week 1-2):**
- **Remote Mixing & Mastering Storefront** (Score: 91) — implement now

**🟡 Short-Term Expansion (Month 1-2):**
- Online Music Lesson Marketplace (Score: 73)
- Session Musician Marketplace (Score: 67)
- Studio Booking Marketplace (Score: 66)

**🟢 Medium-Term Strategic (Month 3-6):**
- DDEX Distribution Gateway (Score: 66)
- Event Production Checkout (Score: 64)

**🔵 Long-Term Global (Month 6+):**
- Full multi-provider marketplace with external onboarding
- Subscription models (monthly mixing credits, lesson subscriptions)
- AI-assisted mixing pre-processing
- White-label platform for other studios

**❌ Rejected/Postponed:**
- Equipment marketplace expansion — low margin, geographic limits, already built
- Cryptocurrency payments — regulatory uncertainty, low demand
- NFT/music collectibles — market collapsed, no evidence of LATAM demand

---

## 6. Selected Implementation: Remote Mixing & Mastering Storefront

### Rationale
Score of 91/100. Leverages existing infrastructure (pipelines, Kanban, service catalog, payment code). Highest speed-to-revenue. Proven global demand. Zero local competition. Digital delivery = infinite scalability.

### Implementation Scope (Vertical Slice)

#### What the customer experiences:
1. **Landing page** — `/services/mixing-mastering` with clear value proposition, pricing, samples, and CTAs
2. **Service selection** — Choose mixing, mastering, or bundle; select tier (Basic/Pro/Premium)
3. **Order form** — Upload reference tracks, genre, notes, deadline
4. **Checkout** — Pay via Datafast (Ecuador cards) or PayPal (international)
5. **Order tracking** — View status via pipeline stages (Brief → In Progress → v1 Sent → Revisions → Approved → Delivered)
6. **File delivery** — Download delivered mixes/masters
7. **Revision requests** — Request changes within revision policy
8. **Review & reorder** — Rate the service, reorder

#### What TDF staff experiences:
1. **Order inbox** — New orders appear in Kanban pipeline
2. **Status management** — Move orders through stages
3. **File upload** — Upload delivered files
4. **Communication** — Notify customers at each stage
5. **Revenue tracking** — See orders, revenue, completion rates

#### Technical components:
- New `ServiceStorefront` API endpoints (public, no auth required for browsing)
- `ServiceOrder` extension for public orders with file uploads
- Datafast checkout integration (existing DTO, needs handler)
- PayPal checkout integration (existing DTO, needs handler)
- Public-facing React pages (landing, order form, tracking)
- Email notifications at each pipeline stage
- Analytics events for conversion tracking

---

## 7. Payment Audit & Test Matrix

### Current Payment Architecture

| Method | Status | Ecuador | International | Notes |
|--------|--------|---------|---------------|-------|
| Cash | ✅ Working | ✅ | ❌ | Manual recording only |
| Bank Transfer | ✅ Working | ✅ | ❌ | Manual verification |
| Card POS | ✅ Working | ✅ | ❌ | Physical terminal |
| Stripe | ⚠️ Code exists | ❌ | ✅ | NOT available in Ecuador |
| PayPal | ⚠️ DTO exists | ✅ | ✅ | Needs handler implementation |
| Datafast | ⚠️ DTO exists | ✅ | ❌ | Needs handler implementation |
| PayPhone | ⚠️ Enum exists | ✅ | ❌ | Needs full implementation |
| Wompi | ❌ Not available | ❌ | ❌ | Not in Ecuador |
| Crypto | ⚠️ Enum exists | ✅ | ✅ | Needs implementation |

### Payment Gaps to Address
1. **Datafast checkout handler** — DTO exists (`DatafastCheckoutDTO`) but no server handler
2. **PayPal checkout handler** — DTO exists (`PaypalCreateDTO`, `PaypalCaptureReq`) but no server handler
3. **Webhook idempotency** — Stripe webhook handler exists but Datafast/PayPal webhooks don't
4. **Multi-currency** — Schema supports it but no conversion logic
5. **Refund workflow** — Stripe refund code exists but no UI or Datafast/PayPal refund

### Test Matrix

| Scenario | Stripe | Datafast | PayPal | Cash |
|----------|--------|----------|--------|------|
| Successful payment | ✅ Tested | 🔴 Needed | 🔴 Needed | ✅ Manual |
| Failed payment | ✅ Tested | 🔴 Needed | 🔴 Needed | N/A |
| Duplicate webhook | ✅ Tested | 🔴 Needed | 🔴 Needed | N/A |
| Refund (full) | ✅ Code exists | 🔴 Needed | 🔴 Needed | 🔴 Needed |
| Refund (partial) | ❌ Not implemented | 🔴 Needed | 🔴 Needed | 🔴 Needed |
| Currency conversion | ❌ Not implemented | 🔴 Needed | 🔴 Needed | N/A |
| Idempotent retry | ✅ Tested | 🔴 Needed | 🔴 Needed | N/A |
| Abandoned checkout | ✅ Tested | 🔴 Needed | 🔴 Needed | N/A |

---

## 8. Deployment & Rollback Checklist

### Pre-Deployment
- [ ] Datafast merchant account configured
- [ ] PayPal business account configured
- [ ] Database migration for service storefront tables
- [ ] Environment variables set (Datafast API keys, PayPal credentials)
- [ ] Webhook endpoints configured (Datafast IPN, PayPal webhooks)
- [ ] Email templates reviewed
- [ ] Smoke test plan documented

### Deployment Steps
1. Run database migration
2. Deploy backend with new endpoints
3. Deploy frontend with storefront pages
4. Configure Datafast/PayPal webhooks
5. Run smoke tests (test payment flow end-to-end)
6. Enable feature flag for public access

### Rollback Procedure
1. Disable feature flag (immediate)
2. Revert frontend deployment
3. Revert backend deployment
4. No database rollback needed (additive changes only)

### Risks Requiring Manual Review
- Datafast API credentials must be obtained from Datafast directly
- PayPal webhook signing requires account configuration
- First live transaction should be monitored manually
- Engineer capacity must be verified before marketing push

---

## 9. Measurement Plan & KPIs

### Primary KPIs
| Metric | Target (Month 1) | Target (Month 3) | Target (Month 6) |
|--------|-------------------|-------------------|-------------------|
| Monthly orders | 10 | 50 | 200 |
| Average order value | $120 | $140 | $160 |
| Monthly revenue | $1,200 | $7,000 | $32,000 |
| Conversion rate (visit→order) | 3% | 5% | 7% |
| Payment completion rate | 80% | 90% | 95% |
| Customer satisfaction | 4.5/5 | 4.7/5 | 4.8/5 |
| Repeat purchase rate | 15% | 25% | 35% |
| Refund rate | <5% | <3% | <2% |

### Analytics Events to Track
- `service_page_view` — Landing page visits
- `service_package_selected` — Package selection
- `checkout_started` — Order form submission
- `payment_started` — Payment initiated
- `payment_completed` — Payment successful
- `payment_failed` — Payment failed
- `order_created` — Order confirmed
- `order_delivered` — Service delivered
- `revision_requested` — Customer requested changes
- `review_submitted` — Customer left review
- `reorder_completed` — Repeat purchase

---

## 10. Remaining Risks & Next Actions

### Risks
1. **Datafast onboarding** — May take 1-2 weeks to get merchant credentials
2. **Engineer capacity** — Current team may not handle 50+ orders/month
3. **Quality control** — Remote delivery requires clear quality standards
4. **Dispute resolution** — No formal process for dissatisfied customers
5. **Stripe dependency** — Existing code assumes Stripe; must refactor for multi-provider

### Recommended Next Actions
1. **Immediate (This week):** Implement the mixing/mastering storefront vertical slice
2. **Week 2:** Onboard Datafast merchant account, configure PayPal business
3. **Week 3:** Launch beta with 5-10 existing clients for feedback
4. **Week 4:** Public launch with targeted marketing to Ecuadorian artists
5. **Month 2:** Add online lessons marketplace (second-highest score)
6. **Month 3:** Open provider onboarding for external mixing engineers
7. **Month 4:** Implement DDEX distribution gateway
8. **Month 6:** Full multi-provider marketplace with subscriptions

---

*This report is based on repository analysis as of August 4, 2026, and current market research. Payment provider availability should be re-verified before production deployment.*

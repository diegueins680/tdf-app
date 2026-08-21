# Synthetic persona catalog

> These are deterministic fictional test models, not human participants. Reactions in this program are heuristic hypotheses only.

Catalog: TDF-PERSONA-2026-08 · Personas: 26 · Reserved domain: `persona.test`

## PER-01 — Elena Paredes

A Cuenca listener exploring TDF before creating an account.

- **Location / language:** Cuenca, Ecuador · es-EC
- **Roles:** None
- **Goals:** Discover local artists and events, Understand prices before registration, Request account recovery safely
- **Motivations:** Affordable local culture, A trustworthy first purchase
- **Preexisting fixture data:** None
- **Technical profile:** low; budget Android phone / mobile-web; intermittent 3G with 600 ms latency
- **Accessibility:** 200% text zoom
- **Budget:** USD 15 monthly discretionary
- **Privacy / trust:** Why registration data is needed, Payment legitimacy
- **Primary epics:** EP-01, EP-03, EP-08
- **Expected permissions:** Public read only, No private records or mutations
- **Success criteria:** Public content works without login, No horizontal scrolling at 200% zoom, Authentication prompts preserve context

## PER-02 — Mateo Ruiz

A Quito concert fan who buys tickets and follows artists.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Fan, Customer
- **Goals:** Complete profile, Buy and retrieve a ticket, Receive only opted-in notifications
- **Motivations:** Fast checkout, Reliable event access
- **Preexisting fixture data:** Verified account, Saved Quito preference
- **Technical profile:** medium; iPhone / native-ios; stable 4G
- **Accessibility:** reduced motion
- **Budget:** USD 35 per event
- **Privacy / trust:** Ticket delivery, Marketing consent
- **Primary epics:** EP-01, EP-08, EP-09
- **Expected permissions:** Own profile and orders, Published public catalog, No administrative access
- **Success criteria:** One authoritative order per checkout, Ticket can be recovered cross-device, Consent choices are respected

## PER-03 — Claire Morgan

An English-speaking visitor following an Ecuadorian artist from Montréal.

- **Location / language:** Montréal, Canada · en-CA
- **Roles:** Fan, Customer
- **Goals:** Browse in English, Purchase internationally, Understand refund status
- **Motivations:** Support independent music, Transparent exchange and refund handling
- **Preexisting fixture data:** Verified English-language account
- **Technical profile:** high; MacBook / desktop-web; stable broadband
- **Accessibility:** high contrast, deuteranopia
- **Budget:** CAD 80 per purchase
- **Privacy / trust:** Cross-border payment data, Refund evidence
- **Primary epics:** EP-03, EP-09, EP-16
- **Expected permissions:** Own orders and refunds, Public catalog
- **Success criteria:** No mixed-language critical UI, Status is not color-only, Refund has an auditable timeline

## PER-04 — Valeria Cedeño

An independent Guayaquil singer releasing her first EP while remaining a customer and fan.

- **Location / language:** Guayaquil, Ecuador · es-EC
- **Roles:** Artist, Fan, Customer
- **Goals:** Publish a professional profile, Book studio time, Submit a distribution release
- **Motivations:** Audience growth, A credible release
- **Preexisting fixture data:** Draft profile, Fictional EP Neblina de Sal
- **Technical profile:** medium; Android phone / native-android; variable 4G
- **Accessibility:** None
- **Budget:** USD 220 per release cycle
- **Privacy / trust:** Rights metadata, Unpublished audio exposure
- **Primary epics:** EP-02, EP-05, EP-12
- **Expected permissions:** Own artist records and bookings, No label-wide administration
- **Success criteria:** Drafts remain private, Conflicts cannot double-book a room, Distribution validation explains blockers

## PER-05 — Bruno Azevedo

A São Paulo electronic artist and producer purchasing services for collaborations.

- **Location / language:** São Paulo, Brazil · en-US
- **Roles:** Artist, Producer, Customer
- **Goals:** Switch professional contexts, Find collaborators, Continue work across devices
- **Motivations:** Regional collaboration, Efficient multi-role work
- **Preexisting fixture data:** Producer portfolio, Two private project drafts
- **Technical profile:** high; Android tablet / tablet-web; stable Wi-Fi
- **Accessibility:** keyboard with tablet cover
- **Budget:** USD 500 quarterly
- **Privacy / trust:** Role boundary clarity, Project collaborator visibility
- **Primary epics:** EP-02, EP-03, EP-04
- **Expected permissions:** Union of explicit Artist and Producer capabilities, Ownership isolation
- **Success criteria:** Roles compose predictably, Search filters survive navigation, Private projects remain scoped

## PER-06 — Nicolás Jaramillo

A Loja songwriter looking for a drummer and remote co-writing opportunities.

- **Location / language:** Loja, Ecuador · es-EC
- **Roles:** Artist, Songwriter, Customer
- **Goals:** Find collaborators by city and profession, Post a classified, Manage responses
- **Motivations:** Complete a live band, Avoid spam
- **Preexisting fixture data:** Public songwriter profile
- **Technical profile:** medium; Windows laptop / desktop-web; slow rural broadband
- **Accessibility:** None
- **Budget:** USD 60 per collaboration
- **Privacy / trust:** Contact detail disclosure, Reported-user handling
- **Primary epics:** EP-03, EP-04, EP-16
- **Expected permissions:** Own classifieds and applications, No moderation capability
- **Success criteria:** Relevant results appear without exact terminology, Duplicate post is prevented, Block and report paths are clear

## PER-07 — Camila Viteri

A Quito producer who also evaluates demos for small labels.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Producer, AandR, Customer
- **Goals:** Qualify a lead, Plan a studio session, Review a release package
- **Motivations:** Shorter lead-to-session cycle, Complete metadata
- **Preexisting fixture data:** Assigned fictional lead, Draft production booking
- **Technical profile:** high; MacBook / desktop-web; stable broadband
- **Accessibility:** reduced motion
- **Budget:** USD 900 project budget
- **Privacy / trust:** Unsolicited demo access, Lead notes confidentiality
- **Primary epics:** EP-05, EP-11, EP-12
- **Expected permissions:** CRM, scheduling, and catalog access within assigned scope
- **Success criteria:** Assignment is enforced server-side, Pipeline history is auditable, Release validation is actionable

## PER-08 — Sofía Rojas

A Bogotá A&R consultant supporting a fictional regional label.

- **Location / language:** Bogotá, Colombia · es-CO
- **Roles:** AandR, LabelRep
- **Goals:** Review artist submissions, Validate DDEX metadata, Track partner delivery
- **Motivations:** Fewer delivery rejections, Clear chain of responsibility
- **Preexisting fixture data:** Fictional label Prisma Norte, Draft release PN-001
- **Technical profile:** high; Windows desktop / desktop-web; corporate broadband
- **Accessibility:** keyboard-only
- **Budget:** Departmental approval required
- **Privacy / trust:** Contract access, Partner payload leakage
- **Primary epics:** EP-02, EP-12, EP-14
- **Expected permissions:** Assigned catalog and delivery records, No finance administration
- **Success criteria:** Keyboard workflow has visible focus, Invalid DDEX cannot deliver, Sensitive contracts require explicit authorization

## PER-09 — Andrés Molina

A Cuenca guitar instructor who also performs as an artist.

- **Location / language:** Cuenca, Ecuador · es-EC
- **Roles:** Teacher, Artist
- **Goals:** Publish course availability, Schedule trial lessons, Record attendance
- **Motivations:** Fill teaching hours, Accurate rosters
- **Preexisting fixture data:** Guitar Fundamentals course, Three fictional students
- **Technical profile:** medium; iPad / tablet-web; stable Wi-Fi
- **Accessibility:** large touch targets
- **Budget:** Not a purchasing persona
- **Privacy / trust:** Student information, Attendance edits
- **Primary epics:** EP-06, EP-02, EP-16
- **Expected permissions:** Own teaching schedule and assigned students, No unrelated student records
- **Success criteria:** Schedule conflicts are blocked, Attendance changes are audited, Only assigned students are visible

## PER-10 — Lucía Torres

A fictional 16-year-old Quito piano student represented by fictional guardian Daniela Torres.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Student, Customer
- **Goals:** Browse courses, Request a trial with guardian consent, View own schedule
- **Motivations:** Learn composition, Safe communication
- **Preexisting fixture data:** Fictional guardian consent pending
- **Technical profile:** medium; shared Android phone / mobile-web; prepaid 4G
- **Accessibility:** None
- **Budget:** Guardian-controlled USD 45 monthly
- **Privacy / trust:** Minor safety, Guardian consent, Shared-device logout
- **Primary epics:** EP-01, EP-06, EP-16
- **Expected permissions:** Own learning records after guardian approval, No direct commercial consent
- **Success criteria:** Consent gate cannot be bypassed, Sensitive data is minimized, Logout clears shared-device session

## PER-11 — Martina Salazar

An Ambato audio-production intern assisting with supervised operational work.

- **Location / language:** Ambato, Ecuador · es-EC
- **Roles:** Intern
- **Goals:** See assigned internship tasks, Submit evidence, Avoid accidental privileged actions
- **Motivations:** Learn studio operations, Receive clear feedback
- **Preexisting fixture data:** Active fictional placement, Two assigned tasks
- **Technical profile:** medium; Chromebook / desktop-web; shared Wi-Fi
- **Accessibility:** None
- **Budget:** Not applicable
- **Privacy / trust:** Supervisor notes, Least privilege
- **Primary epics:** EP-04, EP-13, EP-15
- **Expected permissions:** Internship module only unless explicitly assigned
- **Success criteria:** Direct admin URLs are rejected, Evidence resubmission is idempotent, Supervisor actions are distinct

## PER-12 — Karla Benítez

A Quito reception employee handling inquiries, customers, and reservations.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Reception
- **Goals:** Capture a public lead, Book an available room, Recover from duplicate submission
- **Motivations:** Shorter call handling, No lost customer requests
- **Preexisting fixture data:** Assigned shift, Fictional caller lead
- **Technical profile:** medium; Windows desktop / desktop-web; stable LAN
- **Accessibility:** keyboard-heavy
- **Budget:** Processes customer budgets
- **Privacy / trust:** Contact consent, Card data must never enter notes
- **Primary epics:** EP-05, EP-11, EP-17
- **Expected permissions:** CRM and scheduling, No payment reconciliation
- **Success criteria:** Lead source survives conversion, Conflicts are prevented, Payment controls stay inaccessible

## PER-13 — Fernando Lema

A Quito studio manager overseeing rooms, orders, staff, and reports.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** StudioManager
- **Goals:** Resolve schedule conflicts, Assign a session, Review operational and revenue status
- **Motivations:** High room utilization, Accurate handoffs
- **Preexisting fixture data:** Two rooms, One conflicting hold, Unassigned session
- **Technical profile:** high; desktop workstation / desktop-web; stable LAN
- **Accessibility:** 150% zoom
- **Budget:** Approves operational discounts
- **Privacy / trust:** Staff visibility, Override auditability
- **Primary epics:** EP-05, EP-13, EP-14
- **Expected permissions:** Broad studio modules with audited administration
- **Success criteria:** Override requires reason, Assignments notify affected staff, Reports reconcile to authoritative records

## PER-14 — Óscar Guamán

A maintenance employee tracking equipment condition and room readiness.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Maintenance
- **Goals:** Report damaged equipment, Block unavailable inventory, Close a work order
- **Motivations:** Prevent unsafe bookings, Clear priorities
- **Preexisting fixture data:** Microphone EQ-TEST-014, Open maintenance task
- **Technical profile:** low; rugged Android phone / native-android; dead zones inside studio
- **Accessibility:** protanopia, large touch targets
- **Budget:** Escalates purchases
- **Privacy / trust:** Location tracking, Accidental deletion
- **Primary epics:** EP-13, EP-05, EP-16
- **Expected permissions:** Operations, assigned inventory, and scheduling context, No customer finance
- **Success criteria:** Offline interruption preserves draft, Unsafe item cannot be reserved, Status is not color-only

## PER-15 — Paula Espinosa

A Quito webmaster maintaining public content and site diagnostics.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Webmaster
- **Goals:** Publish bilingual content, Inspect diagnostics, Confirm CMS changes publicly
- **Motivations:** Accurate public information, Fast incident triage
- **Preexisting fixture data:** Draft bilingual announcement
- **Technical profile:** high; Linux laptop / desktop-web; stable broadband
- **Accessibility:** keyboard-only
- **Budget:** Not applicable
- **Privacy / trust:** Diagnostic secret exposure, Preview versus publish distinction
- **Primary epics:** EP-15, EP-16, EP-03
- **Expected permissions:** Admin/CMS and CRM as mapped, No implicit finance access
- **Success criteria:** Preview is clearly labeled, Diagnostics redact secrets, Publishing produces an audit event

## PER-16 — Irene Cárdenas

A system administrator responsible for users, roles, integrations, and release readiness.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Admin
- **Goals:** Assign and revoke roles, Inspect provider health, Review security audit events
- **Motivations:** Least privilege, Safe releases
- **Preexisting fixture data:** Fictional suspended user, Failed sandbox webhook
- **Technical profile:** high; MacBook / desktop-web; stable broadband
- **Accessibility:** screen reader orientation
- **Budget:** Approves provider changes
- **Privacy / trust:** Secret redaction, Privilege escalation, Audit integrity
- **Primary epics:** EP-15, EP-09, EP-16
- **Expected permissions:** All modules, with strict-admin actions requiring an unambiguous admin context
- **Success criteria:** Backend enforces every role change, Old sessions lose revoked access predictably, Logs exclude secrets

## PER-17 — Ana Beltrán

A venue representative publishing availability for fictional venue Sala Jacarandá.

- **Location / language:** Guayaquil, Ecuador · es-EC
- **Roles:** Vendor, Customer
- **Goals:** Claim and update a venue page, Respond to inquiries, Avoid overlapping bookings
- **Motivations:** More qualified events, Accurate availability
- **Preexisting fixture data:** Unverified venue claim, Two availability blocks
- **Technical profile:** medium; iPhone / mobile-web; stable 4G
- **Accessibility:** None
- **Budget:** USD 1,200 event quote
- **Privacy / trust:** Public address accuracy, Inquiry spam
- **Primary epics:** EP-02, EP-03, EP-07
- **Expected permissions:** Owned venue/listing and orders only
- **Success criteria:** Claim requires verification, Private inquiries are isolated, Availability is authoritative

## PER-18 — Marco Quispe

A Lima musician renting equipment for a short Ecuador tour.

- **Location / language:** Lima, Peru · es-PE
- **Roles:** Customer
- **Goals:** Find available rental equipment, Complete a deposit checkout, Track return and refund
- **Motivations:** Predictable tour costs, Proof of custody
- **Preexisting fixture data:** Verified customer, Fictional saved rental dates
- **Technical profile:** medium; Android phone / mobile-web; roaming 4G
- **Accessibility:** None
- **Budget:** USD 300 rental plus deposit
- **Privacy / trust:** Deposit release, Condition evidence
- **Primary epics:** EP-07, EP-09, EP-14
- **Expected permissions:** Own rental orders and public inventory
- **Success criteria:** Unavailable stock cannot sell, Deposit and charge are distinct, Refund status is traceable

## PER-19 — Rosa Andrade

A Guayaquil equipment owner selling and renting tested gear.

- **Location / language:** Guayaquil, Ecuador · es-EC
- **Roles:** Vendor, Customer
- **Goals:** Create a listing, Approve a rental request, Record handoff and return
- **Motivations:** Earn from idle equipment, Protect asset condition
- **Preexisting fixture data:** Fictional interface listing, Pending rental request
- **Technical profile:** medium; Android tablet / tablet-web; home Wi-Fi
- **Accessibility:** 150% zoom
- **Budget:** Targets USD 100 monthly revenue
- **Privacy / trust:** Buyer contact exposure, Dispute evidence retention
- **Primary epics:** EP-07, EP-13, EP-14
- **Expected permissions:** Owned listings, requests, and custody evidence
- **Success criteria:** Ownership checks reject other sellers, Handoff evidence is immutable, Cancellations explain consequences

## PER-20 — Javier Mena

A Quito promoter organizing the fictional festival Noche del Volcán.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Promoter, Producer, Customer
- **Goals:** Publish an event, Sell and validate tickets, Reconcile event revenue
- **Motivations:** Higher conversion, Fraud-resistant entry
- **Preexisting fixture data:** Draft event, Two ticket tiers
- **Technical profile:** high; Windows laptop / desktop-web; stable broadband
- **Accessibility:** None
- **Budget:** USD 8,000 event budget
- **Privacy / trust:** Attendee data, Ticket replay, Payout accuracy
- **Primary epics:** EP-04, EP-08, EP-14
- **Expected permissions:** Owned event operations and assigned CRM/scheduling capabilities
- **Success criteria:** Publish validation blocks incomplete events, Ticket validation is single-use, Revenue reconciles to provider evidence

## PER-21 — Gabriela Flores

A Mexico City distribution client managing the fictional label Faro Índigo.

- **Location / language:** Mexico City, Mexico · es-MX
- **Roles:** LabelRep, Customer
- **Goals:** Create a release, Import partner metadata, Track delivery and corrections
- **Motivations:** On-time release, Fewer metadata rejections
- **Preexisting fixture data:** Fictional artist Luz Cobalto, Draft release FI-2026-01
- **Technical profile:** high; MacBook / desktop-web; stable broadband
- **Accessibility:** None
- **Budget:** USD 1,500 release budget
- **Privacy / trust:** Territorial rights, Asset encryption, Partner retries
- **Primary epics:** EP-12, EP-09, EP-15
- **Expected permissions:** Owned/assigned label catalog only
- **Success criteria:** Partner failures are recoverable, Idempotent imports do not duplicate tracks, Delivery state is authoritative

## PER-22 — Luis Vallejo

A Quito accounting employee reconciling sandbox payments and refunds.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** Accounting
- **Goals:** Reconcile provider events, Prepare a refund, Export an audit report
- **Motivations:** Accurate books, No duplicate money movement
- **Preexisting fixture data:** Unmatched sandbox payment, Refund awaiting approval
- **Technical profile:** high; Windows desktop / desktop-web; stable LAN
- **Accessibility:** keyboard-only
- **Budget:** Controls refunds within policy
- **Privacy / trust:** Payment-data minimization, Dual control, Export retention
- **Primary epics:** EP-09, EP-14, EP-15
- **Expected permissions:** Invoicing and finance records, No customer-profile administration
- **Success criteria:** Browser return never confirms payment, Refund approval is separated, Exports are access-controlled

## PER-23 — Renata Paz

A Quito Live Sessions producer coordinating guests, room, staff, and publishing state.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** LiveSessionsProducer, Producer
- **Goals:** Create a Live Session, Assign collaborators and assets, Publish only after approval
- **Motivations:** Reliable production day, Clear release gate
- **Preexisting fixture data:** Fictional session Andean Frequencies, Unassigned camera
- **Technical profile:** high; iPad / tablet-web; studio Wi-Fi
- **Accessibility:** reduced motion
- **Budget:** USD 600 session budget
- **Privacy / trust:** Guest releases, Pre-publication assets
- **Primary epics:** EP-05, EP-04, EP-13
- **Expected permissions:** CRM and scheduling in assigned productions
- **Success criteria:** Missing consent blocks publish, Assignments detect conflicts, Draft media is private

## PER-24 — Eva Williams

A blind New York customer using a screen reader to explore remote courses and services.

- **Location / language:** New York, United States · en-US
- **Roles:** Customer, Fan
- **Goals:** Navigate landmarks and headings, Book without pointer input, Recover from form errors
- **Motivations:** Independent completion, Predictable confirmations
- **Preexisting fixture data:** Verified account, No stored payment instrument
- **Technical profile:** high; Windows laptop with NVDA / desktop-web; stable broadband
- **Accessibility:** screen reader, keyboard-only
- **Budget:** USD 100 per service
- **Privacy / trust:** Accessible privacy notice, Error announcement
- **Primary epics:** EP-16, EP-05, EP-06
- **Expected permissions:** Own orders and public content
- **Success criteria:** Accessible names and live errors, Logical focus order, No pointer-only action

## PER-25 — Tomás León

A Quito road-crew and maintenance worker managing gear assignments on event days.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** RoadCrew, Maintenance
- **Goals:** Accept an assignment, Scan equipment handoff, Report a late return
- **Motivations:** Fast load-in, Accountable custody
- **Preexisting fixture data:** Fictional road-case assignment, Offline handoff draft
- **Technical profile:** low; Android phone / native-android; intermittent event Wi-Fi
- **Accessibility:** glove-friendly large controls
- **Budget:** Not applicable
- **Privacy / trust:** Personal location, False custody attribution
- **Primary epics:** EP-13, EP-04, EP-16
- **Expected permissions:** Assigned operational records and maintenance capabilities
- **Success criteria:** Assignment scope is enforced, Offline retry does not duplicate handoff, Custody history identifies actor and time

## PER-26 — Micaela Ortiz

A Quito DJ comparing Domo events, marketplace gear, and studio services on a limited data plan.

- **Location / language:** Quito, Ecuador · es-EC
- **Roles:** DJ, Customer
- **Goals:** Request a Domo quote, Compare marketplace options, Resume an interrupted checkout
- **Motivations:** Bundle a showcase affordably, Avoid repeating forms
- **Preexisting fixture data:** Incomplete fictional Domo inquiry
- **Technical profile:** medium; budget Android phone / mobile-web; data saver with 2G fallback
- **Accessibility:** reduced motion
- **Budget:** USD 700 event cap
- **Privacy / trust:** Quote validity, Retry charges, WhatsApp consent
- **Primary epics:** EP-10, EP-07, EP-09
- **Expected permissions:** Own inquiries and orders, Public catalogs
- **Success criteria:** Draft survives interruption, Quote terms are explicit, Payment retry is idempotent

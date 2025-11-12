# Lesson & Package Management Endpoint Clarifications

To design the full set of endpoints for lessons, schedules, payments, and related resources, please help clarify the following:

## Domain & General Requirements
- Which user roles exist in the system (e.g., admin, teacher, student) and what permissions should each role have for these endpoints?
- Are there separate authentication/authorization mechanisms already in place that we should integrate with?
- Should all endpoints be versioned (e.g., `/api/v1/...`) or follow an existing routing convention?

## Lessons
- What core fields should a lesson include (e.g., subject, start/end time, duration, delivery mode, location, price, package linkage)?
- Are lessons always tied to a package, or can they be standalone?
- How should recurring lessons be represented, if at all?
- What statuses can a lesson have (scheduled, completed, cancelled, rescheduled, etc.), and what state transitions must be enforced?
- Are there capacity constraints (number of students per lesson) that we need to model?

## Lesson Scheduling & Calendar Views
- How should teachers view their lesson schedules (filters, date ranges, pagination)?
- Do students need a similar schedule view, and should it include both past and upcoming lessons?
- Should we support calendar exports or reminders/notifications when lessons are created or updated?

## Student–Teacher Relationships
- How are students associated with teachers (one-to-many, many-to-many)?
- Should there be endpoints for teachers to invite or approve students, or is the relationship pre-defined elsewhere?
- What data should be displayed when viewing students by teacher (progress metrics, contact info, package status)?

## Packages & Payments
- What constitutes a "package" (number of lessons, validity period, price, discount structure)?
- How are packages purchased and activated? Do we need endpoints for package catalog management?
- Should payments be processed internally or do we integrate with an external payment gateway? If external, which provider and what webhook flows are expected?
- What payment statuses must we track (pending, completed, failed, refunded)?
- How do payments tie to packages and individual lessons? Can a package cover multiple students or teachers?

## Payment Proof & Invoicing
- What information must be stored for payment proofs (receipts, transaction IDs, uploaded documents)?
- Do we need endpoints to upload and manage proof-of-payment files? Any storage constraints or preferred formats?
- Should invoices or payment summaries be generated and downloadable?

## Lesson Materials
- What types of materials are associated with lessons (files, links, notes)?
- Should materials be versioned or have access restrictions per role?
- Do we need endpoints for uploading, updating, and deleting materials? Any size limits or supported file types?

## Notifications & Audit
- Should endpoint activity trigger notifications (email, SMS, in-app) for students or teachers?
- Do we need to log audit trails for changes to lessons, payments, or materials?

## Reporting & Analytics
- Are there dashboard or reporting requirements (e.g., lessons completed per teacher, revenue per package)?
- Should endpoints support aggregation queries or export formats (CSV, PDF)?

## Integrations & External Systems
- Are there existing integrations (LMS, CRM, accounting) that these endpoints must interact with?
- Do we need webhooks for external systems to consume lesson or payment events?

## Performance & Scalability
- Expected volume of lessons, students, and packages to plan for pagination, caching, or rate limiting?
- Any SLA or latency requirements for critical endpoints (e.g., booking lessons, processing payments)?

## Compliance & Privacy
- Are there regulatory requirements (e.g., GDPR) that impact data retention or access controls?
- Should students or teachers be able to request data deletion or export related to lessons and payments?

Please provide as much detail as possible so we can model the resources, define the API contracts, and plan the necessary endpoints accurately.

# New package purchase expiry — PKG-PURCHASE-EXPIRY-001

The legacy PackageProduct/PackagePurchase workflow has product validity days and
purchase expiry timestamps. The user manual (Paquetes de Clases, lines 209–262)
promises validity and an expiry date; the shipped Guitar 24h seed has 120 days.
The operations capture trigger reads the stored expiry for warnings. These are
separate from Trials and the older lesson-package/enrollment schemas.

Owner decisions on 2026-10-30 establish both rules:

- "Use purchase time + elapsed days (Recommended)": a configured positive n means
  exactly n × 86400 elapsed seconds after the stored purchasedAt timestamp.
- "Refuse nonpositive validity (Recommended)": zero and negative configured days
  are invalid product configuration; refuse the new purchase. An absent validity
  value remains non-expiring.

For an authorized new purchase, preserve ID/reference validation and inactive
product refusal. After those existing checks, reject Just n where n <= 0 with
HTTP 409 and no inserted purchase. For an admitted product with Just n, persist
expiresAt = Just (purchasedAt + n × 86400 seconds). With Nothing, persist Nothing.
Snapshot these terms on the new purchase; do not recompute or rewrite old rows.
Preserve the selected buyer/product, price, remaining units and initial status.
Convert n to NominalDiffTime before multiplying; do not multiply bounded Ints.

Acceptance evidence must call the actual createPurchase Handler with a permitted
Admin actor and owned pooled SQLite fixtures, then inspect persisted purchases.
Use stored purchasedAt as the exact oracle anchor, with positive 1,120,365-day
cases, absent/zero/negative terms, and preserved permission/reference failures.
Invalid terms and other refusals must leave the purchase table empty. Controls
must detect missing expiry, wrong duration units and invalid-term admission;
restore source and requalify the final implementation.

Assumptions and limits: trusted compiler/time/Persistent semantics, valid finite
terms within the deployed database's representable timestamp range, isolated
SQLite adapter fixtures, existing module access, and a host wall clock that does
not step between the fixture's before/after timestamp samples. These finite tests are
TESTED, not a complete implementation/refinement proof, production PostgreSQL
trigger execution, authenticated HTTP transport or deployment evidence. No new
payment, actor/ownership, gift, transfer, idempotency or redemption policy is
established. No charge, production incident, actual expired consumption or active
UI display failure is inferred. Very large durations and database timestamp-range
admission remain a separate unresolved boundary; no migration or historical
financial record correction is authorized by this change.

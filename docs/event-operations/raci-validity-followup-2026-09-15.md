# Preserve RACI validity when applying task commit guards

The successor migration now validates `[valid_from, valid_until)` at the actual
validation time, matching the repaired foundation. Applying its CREATE OR REPLACE
definition must not restore authority from expired or future assignments.
Existing event write fences, policy protections, override binding and rollback
refusal remain unchanged. Explicit retirement records actor/reason and preserves
old intervals; replacement must commit in the same transaction.

The regression suite adds actual timed expiry, denial of completion, attributed
replacement/history preservation and future-dated responsibility denial. The
existing RACI races across all isolation levels, both status/dependency race
orders, immediate constraints, rollback and incompatible-data checks still run.
No production manifest, API, feature activation or deployment change is included.

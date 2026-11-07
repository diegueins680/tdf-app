#!/usr/bin/env python3
"""PAY-DOMAIN-BINDING-001: what commerce_checkout_has_verified_payment admits.

The predicate of tdf-hq/sql/2026-10-16_provider_domain_payment_binding.sql is
encoded conjunct by conjunct over one checkout, one payment attempt, one
provider binding and, for hosted providers, one provider operation and one
payment intent. Text columns are abstract values; equality is all the
predicate uses. Provider and resource names are distinct constants.

Proofs (UNSAT of the negation):
  - hosted branch (PlaceToPay session, PayPhone sale): an admitted attempt,
    binding, create operation and intent agree on checkout, provider, account,
    environment, provider reference, provider resource, currency and amount,
    exactly as the requirement states;
  - direct branch (Datafast, PayPal, Stripe): the binding's merchant reference
    is the checkout's domain order id and the resource type is the provider's;
  - an attempt that is not succeeded, or of another environment, amount or
    currency, is never admitted.

Necessity: each hosted-branch conjunct is dropped in turn; the script reports
whether the agreement then fails (the conjunct is load-bearing) or still holds
(it is redundant given the others). Load-bearing conjuncts are pinned: if one
becomes redundant or a redundant one becomes load-bearing, the run fails, so a
change to the encoding has to be reviewed.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
SOURCE = 'tdf-hq/sql/2026-10-16_provider_domain_payment_binding.sql'
SOURCE_SHA256 = 'b6e7cb3bfdb7228af49ef7b6ec5a124c1eae0810ea11cf3ccb9fec24f8b4b764'
DEFINITION = 'FUNCTION commerce_checkout_has_verified_payment'


def source_is_current():
    """The encoding below is reviewed against SOURCE: refuse if SOURCE changed
    or another forward migration redefines the predicate."""
    digest = hashlib.sha256((ROOT / SOURCE).read_bytes()).hexdigest()
    definers = sorted(str(path.relative_to(ROOT)) for path in (ROOT / 'tdf-hq/sql').glob('*.sql')
                      if not path.name.endswith('_rollback.sql') and DEFINITION in path.read_text(encoding='utf-8'))
    ok = digest == SOURCE_SHA256 and definers == [SOURCE]
    print(f"TDF_SMT_RESULT source {SOURCE} sha256={digest} definers={definers} {'ok' if ok else 'FAILED'}")
    return ok

# Provider and resource constants.
P = {name: z3.IntVal(i) for i, name in enumerate(
    ['datafast', 'paypal', 'stripe', 'placetopay', 'payphone', 'bank_transfer', 'cash', 'pos'])}
R = {name: z3.IntVal(100 + i) for i, name in enumerate(
    ['checkout', 'capture', 'payment', 'session', 'sale', 'manual_evidence'])}
SUCCEEDED, CREATE = z3.IntVal(200), z3.IntVal(300)


def fields(prefix, names):
    return {n: z3.Int(f'{prefix}.{n}') for n in names}


checkout = fields('checkout', ['id', 'environment', 'total_minor', 'currency', 'domain_order_id'])
attempt = fields('attempt', ['id', 'checkout_id', 'status', 'provider', 'environment', 'merchant_account_ref',
                             'amount_minor', 'currency', 'operation', 'payment_intent_id'])
binding = fields('binding', ['payment_attempt_id', 'provider', 'environment', 'merchant_account_ref',
                             'amount_minor', 'currency', 'merchant_reference', 'resource_type',
                             'provider_resource_id'])
operation = fields('operation', ['payment_attempt_id', 'operation', 'provider', 'environment',
                                 'merchant_account_ref', 'provider_reference', 'provider_resource_id'])
intent = fields('intent', ['id', 'checkout_id', 'provider', 'amount_minor', 'currency'])

# Conjuncts shared by both branches (JOIN binding ... WHERE attempt ...), in SQL order.
COMMON = [
    ('binding.payment_attempt_id = attempt.id', binding['payment_attempt_id'] == attempt['id']),
    ('binding.provider = attempt.provider', binding['provider'] == attempt['provider']),
    ('binding.environment = attempt.environment', binding['environment'] == attempt['environment']),
    ('binding.merchant_account_ref = attempt.merchant_account_ref',
     binding['merchant_account_ref'] == attempt['merchant_account_ref']),
    ('binding.amount_minor = checkout.total_minor', binding['amount_minor'] == checkout['total_minor']),
    ('binding.currency = checkout.currency', binding['currency'] == checkout['currency']),
    ('attempt.checkout_id = checkout.id', attempt['checkout_id'] == checkout['id']),
    ("attempt.status = 'succeeded'", attempt['status'] == SUCCEEDED),
    ('attempt.environment = checkout.environment', attempt['environment'] == checkout['environment']),
    ('attempt.amount_minor = checkout.total_minor', attempt['amount_minor'] == checkout['total_minor']),
    ('attempt.currency = checkout.currency', attempt['currency'] == checkout['currency']),
]

# Hosted branch (PlaceToPay session, PayPhone sale) and its EXISTS subquery.
HOSTED = [
    ('hosted provider and resource type',
     z3.Or(z3.And(attempt['provider'] == P['placetopay'], binding['resource_type'] == R['session']),
           z3.And(attempt['provider'] == P['payphone'], binding['resource_type'] == R['sale']))),
    ("attempt.operation = 'create'", attempt['operation'] == CREATE),
    ('operation.payment_attempt_id = attempt.id', operation['payment_attempt_id'] == attempt['id']),
    ("operation.operation = 'create'", operation['operation'] == CREATE),
    ('operation.provider = attempt.provider', operation['provider'] == attempt['provider']),
    ('operation.environment = attempt.environment', operation['environment'] == attempt['environment']),
    ('operation.merchant_account_ref = attempt.merchant_account_ref',
     operation['merchant_account_ref'] == attempt['merchant_account_ref']),
    ('operation.provider_reference = binding.merchant_reference',
     operation['provider_reference'] == binding['merchant_reference']),
    ('operation.provider_resource_id = binding.provider_resource_id',
     operation['provider_resource_id'] == binding['provider_resource_id']),
    ('intent.id = attempt.payment_intent_id', intent['id'] == attempt['payment_intent_id']),
    ('intent.checkout_id = checkout.id', intent['checkout_id'] == checkout['id']),
    ('intent.provider = attempt.provider', intent['provider'] == attempt['provider']),
    ('intent.amount_minor = checkout.total_minor', intent['amount_minor'] == checkout['total_minor']),
    ('intent.currency = checkout.currency', intent['currency'] == checkout['currency']),
]

DIRECT = [
    ('binding.merchant_reference = checkout.domain_order_id',
     binding['merchant_reference'] == checkout['domain_order_id']),
    ('direct provider and resource type',
     z3.Or(z3.And(attempt['provider'] == P['datafast'], binding['resource_type'] == R['checkout']),
           z3.And(attempt['provider'] == P['paypal'], binding['resource_type'] == R['capture']),
           z3.And(attempt['provider'] == P['stripe'], binding['resource_type'] == R['payment']))),
]


def all_equal(*values):
    return z3.And(*[values[0] == v for v in values[1:]])


# The requirement's agreement for hosted evidence: attempt, binding, create
# operation and intent agree on checkout, provider, account, environment,
# reference, resource, currency and amount.
HOSTED_AGREEMENT = z3.And(
    all_equal(checkout['id'], attempt['checkout_id'], intent['checkout_id']),
    attempt['id'] == binding['payment_attempt_id'], attempt['id'] == operation['payment_attempt_id'],
    intent['id'] == attempt['payment_intent_id'],
    all_equal(attempt['provider'], binding['provider'], operation['provider'], intent['provider']),
    z3.Or(attempt['provider'] == P['placetopay'], attempt['provider'] == P['payphone']),
    all_equal(attempt['merchant_account_ref'], binding['merchant_account_ref'], operation['merchant_account_ref']),
    all_equal(checkout['environment'], attempt['environment'], binding['environment'], operation['environment']),
    operation['provider_reference'] == binding['merchant_reference'],
    operation['provider_resource_id'] == binding['provider_resource_id'],
    z3.Or(z3.And(attempt['provider'] == P['placetopay'], binding['resource_type'] == R['session']),
          z3.And(attempt['provider'] == P['payphone'], binding['resource_type'] == R['sale'])),
    attempt['operation'] == CREATE, operation['operation'] == CREATE,
    all_equal(checkout['currency'], attempt['currency'], binding['currency'], intent['currency']),
    all_equal(checkout['total_minor'], attempt['amount_minor'], binding['amount_minor'], intent['amount_minor']),
    attempt['status'] == SUCCEEDED,
)

DIRECT_AGREEMENT = z3.And(
    binding['merchant_reference'] == checkout['domain_order_id'],
    z3.Or(z3.And(attempt['provider'] == P['datafast'], binding['resource_type'] == R['checkout']),
          z3.And(attempt['provider'] == P['paypal'], binding['resource_type'] == R['capture']),
          z3.And(attempt['provider'] == P['stripe'], binding['resource_type'] == R['payment'])),
    all_equal(checkout['environment'], attempt['environment'], binding['environment']),
    all_equal(checkout['total_minor'], attempt['amount_minor'], binding['amount_minor']),
    all_equal(checkout['currency'], attempt['currency'], binding['currency']),
)

# Conjuncts implied by the others (none today: every hosted-branch conjunct is
# load-bearing; in particular nothing else ties the binding's amount and
# currency to the checkout). Pinned so a change to the encoding is reviewed.
EXPECTED_REDUNDANT = set()


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.add(*hypotheses)
    solver.add(z3.Not(claim) if expect == 'proof' else claim)
    result = solver.check()
    wanted = z3.unsat if expect == 'proof' else z3.sat
    status = 'ok' if result == wanted else 'FAILED'
    print(f'TDF_SMT_RESULT {expect} {name} {result} {status}')
    return result == wanted


def holds(hypotheses, claim):
    solver = z3.Solver()
    solver.add(*hypotheses)
    solver.add(z3.Not(claim))
    return solver.check() == z3.unsat


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    if not source_is_current():
        print('Review the encoding against the changed predicate, then update SOURCE_SHA256.', file=sys.stderr)
        return 1
    distinct = [z3.Distinct(*P.values(), *R.values(), SUCCEEDED, CREATE)]
    common = [c for _, c in COMMON]
    hosted = common + [c for _, c in HOSTED]
    direct = common + [c for _, c in DIRECT]
    results = [
        check('hosted-evidence-agrees', distinct + hosted, HOSTED_AGREEMENT, 'proof'),
        check('direct-evidence-agrees', distinct + direct, DIRECT_AGREEMENT, 'proof'),
        check('hosted-admission-reachable', distinct + hosted, z3.BoolVal(True), 'witness'),
        check('direct-admission-reachable', distinct + direct, z3.BoolVal(True), 'witness'),
        # The internal order id plays no part in hosted admission: a provider
        # reference different from it is admitted (distinct identities).
        check('hosted-reference-distinct-from-order-id', distinct + hosted,
              binding['merchant_reference'] != checkout['domain_order_id'], 'witness'),
    ]
    for label, value in [('not-succeeded', attempt['status'] != SUCCEEDED),
                         ('other-environment', attempt['environment'] != checkout['environment']),
                         ('other-amount', attempt['amount_minor'] != checkout['total_minor']),
                         ('other-currency', attempt['currency'] != checkout['currency'])]:
        results.append(check(f'refuses-{label}', distinct + [z3.Or(z3.And(*hosted), z3.And(*direct)), value],
                             z3.BoolVal(False), 'proof'))
    # Necessity of each conjunct for the hosted agreement.
    every = COMMON + HOSTED
    for index, (label, _) in enumerate(every):
        rest = [c for i, (_, c) in enumerate(every) if i != index]
        redundant = holds(distinct + rest, HOSTED_AGREEMENT)
        expected = label in EXPECTED_REDUNDANT
        status = 'ok' if redundant == expected else 'FAILED'
        print(f"TDF_SMT_RESULT necessity {'redundant' if redundant else 'load-bearing'} {label} {status}")
        results.append(redundant == expected)
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())

#!/usr/bin/env python3
"""PAY-ADAPTER-BINDING-001: what the PlaceToPay query parser admits.

The parser of tdf-hq/src/TDF/Commerce/ProviderAdapter/PlaceToPay.hs
(parseQuery, parseRequestPayment, validateTransaction, validateBindingParser,
placeToPayStatus) is encoded over one session response with up to three
transactions. Text columns are abstract values; equality is all the parser
uses. Statuses are distinct constants; amounts are exact minor units after
decimalToMinor, which is encoded separately over the reals.

Proofs (UNSAT of the negation):
  - a parse that succeeds binds the response to the locator and the stored
    expectation: request id, reference, amount and currency all match;
  - a Succeeded result needs the session APPROVED and exactly one present
    transaction that is APPROVED, explicitly unreversed, with a positive
    internal reference and original and settled amounts both equal to the
    expectation, every other present transaction REJECTED;
  - Declined and Cancelled results carry ProviderConfirmedNoCharge only when
    every present transaction is REJECTED; Pending and Partial are ambiguous;
  - decimalToMinor (minorToDecimal m) = m for every minor amount, and a
    decimal with sub-cent precision is rejected, never rounded.

Necessity: the parser's guards are dropped in turn (the one-approved rule,
the reversal evidence, the settled-amount check, the request-id check); each
must make a proof fail, so a change to the encoding has to be reviewed.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
SOURCES = {
    'tdf-hq/src/TDF/Commerce/ProviderAdapter/PlaceToPay.hs':
        '58979fe9954101dc83c6ee6f24d61c424d0f5a16258b2eeb3a7e76fff71dcea9',
    'tdf-hq/src/TDF/Commerce/ProviderAdapter.hs':
        '16971d54ba7ab0eccd890a33511cf88b9884599e8b5274cb91ef291736a0a720',
}
N = 3  # transactions encoded per response


def source_is_current():
    ok = True
    for path, pinned in SOURCES.items():
        digest = hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
        current = digest == pinned
        ok = ok and current
        print(f"TDF_SMT_RESULT source {path} sha256={digest} {'ok' if current else 'FAILED'}")
    return ok


# Session and transaction statuses (parseStatus values) and reversal evidence.
S = {name: z3.IntVal(i) for i, name in enumerate(
    ['APPROVED', 'REJECTED', 'PENDING', 'PARTIAL', 'EXPIRED', 'CANCELLED', 'OTHER'])}
REFUNDED = {name: z3.IntVal(10 + i) for i, name in enumerate(['absent', 'false', 'true'])}
STATE = {name: z3.IntVal(20 + i) for i, name in enumerate(
    ['Succeeded', 'Declined', 'Cancelled', 'Pending', 'Unknown'])}
CERT = {name: z3.IntVal(30 + i) for i, name in enumerate(
    ['ProviderSucceeded', 'ProviderConfirmedNoCharge', 'ProviderAmbiguous'])}

# Locator and expectation (PaymentLocator, ExpectedPayment).
locator_id, exp_ref, exp_amt, exp_cur = z3.Ints('locator_id exp_ref exp_amt exp_cur')
# Response: request id, session status, request payment binding.
req_id, status, ref, amt, cur = z3.Ints('req_id status ref amt cur')


def tx(i):
    names = ['present', 'status', 'refunded', 'internal_ref', 'ref', 'orig_amt', 'orig_cur',
             'settled_amt', 'settled_cur']
    return {n: z3.Int(f'tx{i}_{n}') for n in names}


TX = [tx(i) for i in range(N)]


def binding_ok(reference, amount, currency):
    # validateBindingParser: reference, amount and currency equal the expectation.
    return z3.And(reference == exp_ref, amount == exp_amt, currency == exp_cur)


def present(t):
    return t['present'] == 1


def status_valid(value):
    return z3.Or(*[value == s for s in S.values()])


# validateTransaction, with named guards so each can be dropped.
def tx_guards(t, drop=()):
    approved = t['status'] == S['APPROVED']
    guards = {
        'not-refunded': t['refunded'] != REFUNDED['true'],
        'approval-unreversed': z3.Implies(approved, t['refunded'] == REFUNDED['false']),
        'approval-internal-reference': z3.Implies(approved, t['internal_ref'] > 0),
        'approval-original-amount': z3.Implies(
            approved, binding_ok(t['ref'], t['orig_amt'], t['orig_cur'])),
        'approval-settled-amount': z3.Implies(
            approved, binding_ok(t['ref'], t['settled_amt'], t['settled_cur'])),
    }
    return z3.And(*[g for name, g in guards.items() if name not in drop])


def count_approved():
    return z3.Sum(*[z3.If(z3.And(present(t), t['status'] == S['APPROVED']), 1, 0) for t in TX])


def fully_rejected():
    return z3.And(*[z3.Implies(present(t), t['status'] == S['REJECTED']) for t in TX])


def any_incomplete():
    return z3.Or(*[z3.And(present(t), t['status'] != S['APPROVED'], t['status'] != S['REJECTED'])
                   for t in TX])


def place_to_pay_status(approved_count, rejected, drop=()):
    """(state, certainty) of placeToPayStatus."""
    one = approved_count == 1 if 'one-approved' not in drop else approved_count >= 1
    state = z3.If(z3.And(status == S['APPROVED'], one), STATE['Succeeded'],
            z3.If(z3.And(status == S['REJECTED'], rejected), STATE['Declined'],
            z3.If(z3.And(z3.Or(status == S['EXPIRED'], status == S['CANCELLED']), rejected),
                  STATE['Cancelled'],
            z3.If(z3.Or(status == S['PENDING'], status == S['PARTIAL']), STATE['Pending'],
                  STATE['Unknown']))))
    certainty = z3.If(state == STATE['Succeeded'], CERT['ProviderSucceeded'],
                z3.If(z3.Or(state == STATE['Declined'], state == STATE['Cancelled']),
                      CERT['ProviderConfirmedNoCharge'], CERT['ProviderAmbiguous']))
    return state, certainty


def parse_succeeds(drop=()):
    """Every `fail` of parseQuery negated, in source order."""
    approved_count = count_approved()
    conditions = [
        z3.Or(req_id == locator_id, z3.BoolVal('request-id' in drop)),
        status_valid(status),
        binding_ok(ref, amt, cur),
        z3.And(*[z3.Implies(present(t), z3.And(status_valid(t['status']), tx_guards(t, drop))) for t in TX]),
        z3.Not(z3.And(status == S['APPROVED'],
                      z3.Or(z3.BoolVal(False) if 'one-approved' in drop else approved_count != 1,
                            any_incomplete()))),
    ]
    return z3.And(*conditions)


def domain():
    return [z3.And(*[z3.Or(t['present'] == 0, t['present'] == 1) for t in TX]),
            z3.And(*[z3.Or(*[t['refunded'] == r for r in REFUNDED.values()]) for t in TX])]


def exactly_one_complete_sale():
    """The requirement's success evidence: one present APPROVED, unreversed
    transaction with matching original and settled amounts; every other
    present transaction REJECTED."""
    def complete(t):
        return z3.And(present(t), t['status'] == S['APPROVED'], t['refunded'] == REFUNDED['false'],
                      t['internal_ref'] > 0, binding_ok(t['ref'], t['orig_amt'], t['orig_cur']),
                      binding_ok(t['ref'], t['settled_amt'], t['settled_cur']))
    options = []
    for i, t in enumerate(TX):
        others = [z3.Implies(present(u), u['status'] == S['REJECTED']) for j, u in enumerate(TX) if j != i]
        options.append(z3.And(complete(t), *others))
    return z3.Or(*options)


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.set(timeout=60_000)
    solver.add(*hypotheses)
    solver.add(z3.Not(claim) if expect == 'proof' else claim)
    result = solver.check()
    wanted = z3.unsat if expect == 'proof' else z3.sat
    print(f"TDF_SMT_RESULT {expect} {name} {result} {'ok' if result == wanted else 'FAILED'}", flush=True)
    return result == wanted


def holds(hypotheses, claim):
    solver = z3.Solver()
    solver.set(timeout=60_000)
    solver.add(*hypotheses)
    solver.add(z3.Not(claim))
    return solver.check() == z3.unsat


def money_checks():
    # minorToDecimal m = m / 100 (scale -2); decimalToMinor d = the integer
    # d * 100 when it is one (toBoundedInteger), otherwise Nothing. The scaled
    # value is the real x = d * 100.
    m, n = z3.Ints('m n')
    x = z3.Real('x')
    return [
        # Round trip: scaling m / 100 back gives an integral value equal to m.
        check('minor-round-trip', [x == z3.ToReal(m) / 100 * 100],
              z3.And(z3.IsInt(x), z3.ToInt(x) == m), 'proof'),
        # A sub-cent decimal has a non-integral scaled value; no minor amount
        # n equals it, so decimalToMinor answers Nothing rather than rounding.
        check('sub-cent-never-admitted', [z3.Not(z3.IsInt(x)), z3.ToReal(n) == x],
              z3.BoolVal(False), 'proof'),
        check('sub-cent-values-exist', [z3.Not(z3.IsInt(x))], z3.BoolVal(True), 'witness'),
    ]


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    if not source_is_current():
        print('Review the encoding against the changed parser, then update SOURCES.', file=sys.stderr)
        return 1
    constants = [z3.Distinct(*S.values(), *REFUNDED.values(), *STATE.values(), *CERT.values())]
    base = constants + domain()
    state, certainty = place_to_pay_status(count_approved(), fully_rejected())
    ok = parse_succeeds()
    results = [
        check('parse-binds-locator-and-expectation', base + [ok],
              z3.And(req_id == locator_id, binding_ok(ref, amt, cur)), 'proof'),
        check('succeeded-needs-exactly-one-complete-sale', base + [ok, state == STATE['Succeeded']],
              z3.And(status == S['APPROVED'], exactly_one_complete_sale()), 'proof'),
        check('no-charge-needs-every-transaction-rejected',
              base + [ok, certainty == CERT['ProviderConfirmedNoCharge']], fully_rejected(), 'proof'),
        check('pending-is-ambiguous', base + [ok, state == STATE['Pending']],
              certainty == CERT['ProviderAmbiguous'], 'proof'),
        check('approved-session-with-two-approved-sales-fails',
              base + [status == S['APPROVED'], count_approved() == 2], z3.Not(ok), 'proof'),
        check('refunded-sale-fails', base + [z3.Or(*[z3.And(present(t), t['refunded'] == REFUNDED['true'])
                                                     for t in TX])], z3.Not(ok), 'proof'),
        check('success-reachable', base + [ok, state == STATE['Succeeded']], z3.BoolVal(True), 'witness'),
        check('declined-reachable', base + [ok, state == STATE['Declined']], z3.BoolVal(True), 'witness'),
    ]
    results += money_checks()
    # Necessity: each dropped guard must break the proof it carries.
    necessity = [
        ('one-approved', 'succeeded-needs-exactly-one-complete-sale',
         lambda d: (parse_succeeds(d), z3.And(status == S['APPROVED'], exactly_one_complete_sale()), 'Succeeded')),
        ('approval-unreversed', 'succeeded-needs-exactly-one-complete-sale',
         lambda d: (parse_succeeds(d), exactly_one_complete_sale(), 'Succeeded')),
        ('approval-settled-amount', 'succeeded-needs-exactly-one-complete-sale',
         lambda d: (parse_succeeds(d), exactly_one_complete_sale(), 'Succeeded')),
        ('request-id', 'parse-binds-locator-and-expectation',
         lambda d: (parse_succeeds(d), req_id == locator_id, None)),
    ]
    for guard, proof, build in necessity:
        dropped_ok, claim, needed_state = build((guard,))
        hyps = base + [dropped_ok]
        if needed_state:
            dropped_state, _ = place_to_pay_status(count_approved(), fully_rejected(), (guard,))
            hyps.append(dropped_state == STATE[needed_state])
        still = holds(hyps, claim)
        print(f"TDF_SMT_RESULT necessity {'redundant' if still else 'load-bearing'} {guard} "
              f"{'FAILED' if still else 'ok'}")
        results.append(not still)
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())

#!/usr/bin/env python3
"""Source-derived Int64 arithmetic obligations; see formal/system/payment-arithmetic.md.

This deliberately recognizes two exact Haskell function shapes. Unsupported source
changes fail closed, rather than silently proving an obsolete parallel function.
"""
import ast
import hashlib
import json
from pathlib import Path
import re
import sys
import z3

ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / "tdf-hq/src/TDF/Commerce/StateMachine.hs"


def expressions(source, name):
    end = "voidAuthorization" if name == "capture" else "invalidPaymentTransition"
    block = source.split(f"{name} ::", 1)[1].split(f"\n{end}", 1)[0]
    block = re.sub(r"(?m)^\s*--[^\n]*", "", block)
    block = " ".join(block.split())
    common = (rf"PaymentLifecycle -> Int64 -> Either Text PaymentLifecycle {name} lifecycle amount "
              r'\| (.+?) = Left "[^"]*" \| (.+?) = Left "[^"]*" '
              r"\| otherwise = Right lifecycle ")
    if name == "capture":
        shape = (r"\{ paymentState = if newCaptured == paymentAmountMinor lifecycle "
                 r"then PaymentCaptured else PaymentPartiallyCaptured , paymentAuthorizedMinor = "
                 r"max \(paymentAuthorizedMinor lifecycle\) maximumCapture , paymentCapturedMinor = newCaptured \} "
                 r"where newCaptured = (.+?) maximumCapture = case paymentState lifecycle of "
                 r"PaymentAuthorized -> paymentAuthorizedMinor lifecycle "
                 r"PaymentPartiallyCaptured -> paymentAuthorizedMinor lifecycle _ -> paymentAmountMinor lifecycle")
    else:
        shape = (r"\{ paymentState = if newRefunded == paymentCapturedMinor lifecycle "
                 r"then PaymentRefunded else PaymentPartiallyRefunded , paymentRefundedMinor = newRefunded \} "
                 r"where newRefunded = (.+)")
    match = re.fullmatch(common + shape, block)
    if not match:
        raise ValueError(f"Unsupported {name} source shape; review the abstraction before updating this checker")
    return match.groups()


def lower(expression, names):
    # Function applications in this fragment are only immutable record selectors.
    expression = re.sub(r"(payment\w+) lifecycle", r"\1", expression)
    try:
        tree = ast.parse(expression, mode="eval")
    except SyntaxError as error:
        raise ValueError(f"Unsupported arithmetic expression: {expression}") from error

    def visit(node):
        if isinstance(node, ast.Expression):
            return visit(node.body)
        if isinstance(node, ast.Name) and node.id in names:
            return names[node.id]
        if isinstance(node, ast.Constant) and type(node.value) is int:
            return z3.BitVecVal(node.value, 64)
        if isinstance(node, ast.BinOp) and isinstance(node.op, (ast.Add, ast.Sub)):
            a, b = visit(node.left), visit(node.right)
            return a + b if isinstance(node.op, ast.Add) else a - b
        if isinstance(node, ast.Compare) and len(node.ops) == 1:
            a, b = visit(node.left), visit(node.comparators[0])
            if isinstance(node.ops[0], ast.Gt):
                return a > b  # Z3 bit-vector signed comparison, matching Int64.
            if isinstance(node.ops[0], ast.LtE):
                return a <= b
        raise ValueError(f"Unsupported arithmetic expression: {expression}")

    return visit(tree)


def obligation(source, name, legacy=False):
    first, second, addition = expressions(source, name)
    current, limit, amount = z3.BitVecs("current limit amount", 64)
    names = {"amount": amount, "maximumCapture": limit}
    names.update({"paymentCapturedMinor": current if name == "capture" else limit,
                  "paymentRefundedMinor": current})
    result = lower(addition, names)
    alias = "newCaptured" if name == "capture" else "newRefunded"
    names[alias] = result
    if legacy:
        second = alias + (" > maximumCapture" if name == "capture" else " > paymentCapturedMinor lifecycle")
    accepted = z3.And(z3.Not(lower(first, names)), z3.Not(lower(second, names)))
    domain = z3.And(current >= 0, limit >= 0)
    # The sum of two signed 64-bit inputs always fits in signed 65 bits.
    # This is exact arithmetic, with no mixed Int/BV search or reduced input domain.
    exact_sum = z3.SignExt(1, current) + z3.SignExt(1, amount)
    valid = z3.And(amount > 0, exact_sum <= z3.SignExt(1, limit))
    preserved = z3.And(z3.SignExt(1, result) == exact_sum,
                       result > current, result <= limit)
    return current, limit, amount, result, domain, accepted, valid, preserved


def check(formula, expected):
    solver = z3.Solver()
    solver.set(timeout=30_000)
    solver.add(formula)
    result = solver.check()
    if str(result) != expected:
        raise AssertionError(f"Expected {expected}, got {result}: {solver.reason_unknown() if result == z3.unknown else solver}")
    return str(solver.model()) if result == z3.sat else "unsat"


def verify(source):
    results = []
    for name in ("capture", "refund"):
        c, limit, amount, _, domain, accepted, valid, preserved = obligation(source, name)
        check(z3.And(domain, accepted, z3.Not(preserved)), "unsat")
        check(z3.And(domain, accepted != valid), "unsat")
        # Non-vacuity: both smallest payment and exact maximum balance can succeed.
        for count, cap, increment in ((0, 1, 1), (1, 2**63 - 1, 2**63 - 2)):
            check(z3.And(domain, accepted, c == count, limit == cap, amount == increment), "sat")
        lc, ll, la, _, ld, lac, _, lp = obligation(source, name, legacy=True)
        counterexample = check(z3.And(ld, lac, z3.Not(lp), lc == 1, ll == 2**63 - 1, la == 2**63 - 1), "sat")
        results.append({"function": name, "safety": "unsat", "admission_equivalence": "unsat",
                        "non_vacuity": "sat", "legacy_counterexample": counterexample})
    return results


if __name__ == "__main__":
    if len(sys.argv) != 1:
        raise SystemExit("No options supported; verifies the actual repository source")
    source_bytes = SOURCE.read_bytes()
    if z3.get_version_string() != "4.13.3":
        raise SystemExit("Use the pinned z3-solver==4.13.3.0 environment")
    print(json.dumps({"source": str(SOURCE.relative_to(ROOT)),
                      "sha256": hashlib.sha256(source_bytes).hexdigest(),
                      "solver": z3.get_version_string(), "results": verify(source_bytes.decode()),
                      "boundary": "Two recognized Haskell arithmetic fragments, all signed Int64 inputs with nonnegative current and limit; trusted shallow translation; not SQL or whole-program refinement"}, indent=2))

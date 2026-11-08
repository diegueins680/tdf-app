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


def caller_obligation(source, name):
    """Lower the complete outer validator; no nonnegative-input assumption.

    State membership selects the two recognized capture ceilings. The Boolean is
    arbitrary, so both branches are covered without a small-state enumeration.
    Dispatch/reachability and translation correctness remain explicit boundaries.
    """
    blocks = re.findall(r"(?ms)^transitionPayment\n(.*?)^applyPaymentEvent\n", source)
    if len(blocks) != 1:
        raise ValueError("Expected one complete transitionPayment validator")
    block = " ".join(blocks[0].split())
    pattern = (
        r':: PaymentLifecycle -> PaymentEvent -> Either Text PaymentLifecycle '
        r'transitionPayment lifecycle event '
        r'\| (.+?) = Left "[^"\n]*" '
        r'\| any \(< 0\) \[ (.+?) \] = Left "[^"\n]*" '
        r'\| (.+?) = Left "[^"\n]*" '
        r'\| (.+?) = Left "[^"\n]*" '
        r'\| (.+?) = Left "[^"\n]*" '
        r'\| otherwise = applyPaymentEvent lifecycle event'
    )
    match = re.fullmatch(pattern, block)
    if not match:
        raise ValueError("Unsupported complete transitionPayment validator")
    fields = {field: z3.BitVec(field, 64) for field in (
        "paymentAmountMinor", "paymentAuthorizedMinor", "paymentCapturedMinor",
        "paymentRefundedMinor")}
    first, checked_fields, *guards = match.groups()
    selectors = checked_fields.split(" , ")
    if not selectors or any(s not in {f + " lifecycle" for f in fields} for s in selectors):
        raise ValueError("Unsupported nonnegative-field selectors")
    rejected = [lower(first, fields), *(lower(guard, fields) for guard in guards)]
    rejected.extend(lower(selector, fields) < 0 for selector in selectors)
    admitted = z3.Not(z3.Or(*rejected))
    # This also validates the full existing capture/refund helper shapes,
    # including their ceiling selectors; unknown changes cannot inherit a proof.
    expressions(source, name)
    if name == "capture":
        selected = z3.Bool("capture_uses_authorization_ceiling")
        current = fields["paymentCapturedMinor"]
        ceiling = z3.If(selected, fields["paymentAuthorizedMinor"], fields["paymentAmountMinor"])
    elif name == "refund":
        current, ceiling = fields["paymentRefundedMinor"], fields["paymentCapturedMinor"]
    else:
        raise ValueError("Unsupported helper")
    return admitted, z3.And(current >= 0, ceiling >= 0), fields


def check(formula, expected, *, evidence=None, identity=None):
    solver = z3.Solver()
    solver.set(timeout=30_000)
    solver.add(formula)
    if evidence is not None:
        # Retain the exact solver input, including timeout and expected result.
        problem = f"(set-option :timeout 30000)\n(set-logic QF_BV)\n{solver.sexpr()}\n(check-sat)\n"
        evidence.append({"id": identity, "expected": expected, "smtlib": problem})
    result = solver.check()
    if evidence is not None:
        evidence[-1]["actual"] = str(result)
        evidence[-1]["model"] = str(solver.model()) if result == z3.sat else None
    if str(result) != expected:
        raise AssertionError(f"Expected {expected}, got {result}: {solver.reason_unknown() if result == z3.unknown else solver}")
    return str(solver.model()) if result == z3.sat else "unsat"


def verify(source, evidence=None):
    results = []
    for name in ("capture", "refund"):
        c, limit, amount, _, domain, accepted, valid, preserved = obligation(source, name)
        check(z3.And(domain, accepted, z3.Not(preserved)), "unsat", evidence=evidence, identity=name + "-safety")
        check(z3.And(domain, accepted != valid), "unsat", evidence=evidence, identity=name + "-admission")
        # Non-vacuity: both smallest payment and exact maximum balance can succeed.
        for count, cap, increment in ((0, 1, 1), (1, 2**63 - 1, 2**63 - 2)):
            check(z3.And(domain, accepted, c == count, limit == cap, amount == increment), "sat",
                  evidence=evidence, identity=f"{name}-witness-{count}")
        lc, ll, la, _, ld, lac, _, lp = obligation(source, name, legacy=True)
        counterexample = check(z3.And(ld, lac, z3.Not(lp), lc == 1, ll == 2**63 - 1, la == 2**63 - 1), "sat",
                               evidence=evidence, identity=name + "-legacy-counterexample")
        admitted, precondition, fields = caller_obligation(source, name)
        check(z3.And(admitted, z3.Not(precondition)), "unsat", evidence=evidence,
              identity=name + "-caller-precondition")
        check(z3.And(admitted, precondition, fields["paymentAmountMinor"] == 2**63 - 1,
                     *(value == 0 for field, value in fields.items() if field != "paymentAmountMinor")),
              "sat", evidence=evidence, identity=name + "-caller-witness")
        # Remove one genuine source selector. Negative values must yield a real
        # SAT violation; failure to recognize syntax alone is not this control.
        selector = "paymentAuthorizedMinor" if name == "capture" else "paymentRefundedMinor"
        fragment = ("[ paymentAuthorizedMinor lifecycle\n" if name == "capture"
                    else "      , paymentRefundedMinor lifecycle\n")
        replacement = "[ paymentCapturedMinor lifecycle\n" if name == "capture" else ""
        mutated = source.replace(fragment + ("      , paymentCapturedMinor lifecycle\n" if name == "capture" else ""), replacement)
        if mutated == source:
            raise ValueError("Caller mutation did not change the source: " + selector)
        ma, mp, mf = caller_obligation(mutated, name)
        check(z3.And(ma, z3.Not(mp), mf[selector] == -1), "sat", evidence=evidence,
              identity=name + "-caller-control")
        results.append({"function": name, "safety": "unsat", "admission_equivalence": "unsat",
                        "caller_precondition": "unsat", "caller_non_vacuity": "sat",
                        "caller_mutation": "sat", "non_vacuity": "sat", "legacy_counterexample": counterexample})
    return results


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, help="New directory for independently replayable SMT-LIB inputs")
    args = parser.parse_args()
    if args.output is not None:
        args.output.mkdir()  # Never reuse evidence from a previous attempt.
    source_bytes = SOURCE.read_bytes()
    if z3.get_version_string() != "4.13.3":
        raise SystemExit("Use the pinned z3-solver==4.13.3.0 environment")
    problems = []
    results, failure = [], None
    try:
        results = verify(source_bytes.decode(), problems)
    except Exception as error:
        failure = error
    artifacts = []
    if args.output is not None:
        for problem in problems:
            content = problem["smtlib"].encode()
            name = problem["id"] + ".smt2"
            (args.output/name).write_bytes(content)
            artifacts.append({"id": problem["id"], "file": name, "expected": problem["expected"],
                              "sha256": hashlib.sha256(content).hexdigest(),
                              "actual": problem.get("actual"), "model": problem.get("model")})
    report = {"source": str(SOURCE.relative_to(ROOT)),
              "sha256": hashlib.sha256(source_bytes).hexdigest(),
              "solver": z3.get_version_string(), "results": results, "artifacts": artifacts,
              "executionPassed": failure is None, "error": str(failure) if failure else None,
              "boundary": "Two recognized Haskell arithmetic fragments plus outer-validator precondition implications over all signed Int64 fields; trusted shallow translation; not dispatch, SQL or whole-program refinement"}
    if args.output is not None:
        (args.output/"result.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    if failure is not None:
        raise SystemExit(1)

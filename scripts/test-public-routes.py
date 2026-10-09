#!/usr/bin/env python3
"""Negative controls for scripts/check-public-routes.py (AUTH-PUBLIC-001)."""

import copy
import importlib.util
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location("check_public_routes", ROOT / "scripts/check-public-routes.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)

SURFACE = json.loads((ROOT / "formal/system/compiled-api-surface.json").read_text())["surface"]
REGISTER = json.loads((ROOT / "formal/system/public-routes.json").read_text())


def op(method, path, headers=(), body=None, auth=False):
    return {
        "method": method,
        "path": path,
        "parameters": [{"in": "header", "name": name} for name in headers],
        "bodies": [] if body is None else [{"type": {"name": body}}],
        "authCombinators": [{"name": "AuthProtect"}] if auth else [],
    }


def entry(register, method, path):
    return next(r for r in register["routes"] if r["method"] == method and r["path"] == path)


def expect_rejected(name, surface, register, fragment):
    errors = gate.check(surface, register)
    if not any(fragment in error for error in errors):
        raise AssertionError(f"{name}: expected rejection containing {fragment!r}, got {errors}")


def main():
    assert gate.check(SURFACE, REGISTER) == [], gate.check(SURFACE, REGISTER)

    surface = copy.deepcopy(SURFACE)
    surface["operations"].append(op("POST", "/public/courses/{slug}/registrations/{registrationId}/payment-intent", body="CoursePaymentIntentRequest"))
    expect_rejected("new unauthenticated mutation", surface, REGISTER, "unregistered unauthenticated route")

    surface = copy.deepcopy(SURFACE)
    surface["rawMounts"].append({"path": "/private-export"})
    expect_rejected("new raw mount", surface, REGISTER, "unregistered unauthenticated route: RAW /private-export")

    register = copy.deepcopy(REGISTER)
    register["routes"].append({"method": "POST", "path": "/retired", "boundary": "public-intake"})
    expect_rejected("stale entry", SURFACE, register, "stale register entry")

    surface = copy.deepcopy(SURFACE)
    target = next(o for o in surface["operations"] if o["method"] == "POST" and o["path"] == "/merch/carts/{cartId}/checkout")
    target["authCombinators"] = [{"name": "AuthProtect"}]
    expect_rejected("route became authenticated", surface, REGISTER, "stale register entry")

    register = copy.deepcopy(REGISTER)
    register["routes"].append(dict(entry(register, "POST", "/login")))
    expect_rejected("duplicate", SURFACE, register, "duplicate register entry")

    register = copy.deepcopy(REGISTER)
    entry(register, "POST", "/ads/inquiry")["boundary"] = "public-read"
    expect_rejected("mutation declared read-only", SURFACE, register, "non-mutating boundary")

    surface = copy.deepcopy(SURFACE)
    target = next(o for o in surface["operations"] if o["method"] == "GET" and o["path"] == "/merch/orders/{orderId}")
    target["parameters"] = [p for p in target["parameters"] if p["name"] != "X-Order-Lookup-Token"]
    expect_rejected("lookup token removed", surface, REGISTER, "declared header X-Order-Lookup-Token")

    surface = copy.deepcopy(SURFACE)
    target = next(o for o in surface["operations"] if o["method"] == "POST" and o["path"] == "/social-events/stripe/webhook")
    target["bodies"] = [{"type": {"name": "Value"}}]
    expect_rejected("parsed webhook body", surface, REGISTER, "exact raw body")

    surface = copy.deepcopy(SURFACE)
    target = next(o for o in surface["operations"] if o["method"] == "POST" and o["path"] == "/feedback")
    target["parameters"] = []
    expect_rejected("session headers removed", surface, REGISTER, "requires one of")

    register = copy.deepcopy(REGISTER)
    entry(register, "POST", "/public/whatsapp/consent")["boundary"] = "known-debt"
    expect_rejected("debt without reference", SURFACE, register, "registered debt")

    register = copy.deepcopy(REGISTER)
    entry(register, "POST", "/seed")["header"] = None
    expect_rejected("operator secret undeclared", SURFACE, register, "requires a declared header")

    register = copy.deepcopy(REGISTER)
    entry(register, "GET", "/version")["boundary"] = "trusted"
    expect_rejected("unknown boundary", SURFACE, register, "unknown boundary")

    register = copy.deepcopy(REGISTER)
    entry(register, "RAW", "/assets/serve")["boundary"] = "public-read"
    expect_rejected("raw mount under method-limited boundary", SURFACE, register, "raw-capable boundary")

    register = copy.deepcopy(REGISTER)
    entry(register, "GET", "/version")["boundary"] = "public-static-files"
    expect_rejected("typed route under raw boundary", SURFACE, register, "raw-capable boundary")

    print("Public route admission controls passed (1 positive, 14 negative)")
    return 0


if __name__ == "__main__":
    sys.exit(main())

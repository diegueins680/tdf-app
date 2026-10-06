#!/usr/bin/env python3
"""Admit every compiled route served without AuthProtect (AUTH-PUBLIC-001).

Compares formal/system/compiled-api-surface.json with the reviewed register in
formal/system/public-routes.json. Every unauthenticated operation and Raw mount
must have exactly one register entry, every entry must still exist, and each
entry must satisfy the structural rule of its declared boundary. Passing means
the declaration is reviewed and structurally consistent; it does not prove the
handler enforces the boundary.
"""

import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MUTATING = {"POST", "PUT", "PATCH", "DELETE"}


def load(path):
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


def unauthenticated_surface(surface):
    found = {}
    for op in surface["operations"]:
        if op["authCombinators"]:
            continue
        found[(op["method"], op["path"])] = op
    for mount in surface.get("rawMounts", []):
        found[("RAW", mount["path"])] = mount
    return found


def check(surface, register):
    errors = []
    boundaries = register["boundaries"]
    debts = register.get("debt", {})
    actual = unauthenticated_surface(surface)
    seen = set()
    for entry in register["routes"]:
        key = (entry["method"], entry["path"])
        label = f"{entry['method']} {entry['path']}"
        if key in seen:
            errors.append(f"duplicate register entry: {label}")
            continue
        seen.add(key)
        op = actual.get(key)
        if op is None:
            errors.append(f"stale register entry (route absent or now authenticated): {label}")
            continue
        name = entry.get("boundary")
        rule = boundaries.get(name)
        if rule is None:
            errors.append(f"unknown boundary {name!r}: {label}")
            continue
        method = entry["method"]
        if method in MUTATING and not rule.get("mutating") and entry["path"] not in rule.get("mutatingPaths", []):
            errors.append(f"mutating route cannot use non-mutating boundary {name}: {label}")
        params = op.get("parameters", [])
        headers = {p["name"] for p in params if p["in"] == "header"}
        queries = {p["name"] for p in params if p["in"] == "query"}
        if rule.get("requiresHeader"):
            header = entry.get("header")
            if not header:
                errors.append(f"boundary {name} requires a declared header: {label}")
            elif header not in headers and not (name == "signed-provider-callback" and rule.get("rawBody")):
                errors.append(f"declared header {header} is not an input of {label}")
        if name == "signed-provider-callback":
            header = entry.get("header")
            if header and header not in headers:
                errors.append(f"declared signature header {header} is not an input of {label}")
            bodies = op.get("bodies", [])
            if method in MUTATING and (len(bodies) != 1 or bodies[0]["type"]["name"] != "ByteString"):
                errors.append(f"signed callback must receive the exact raw body: {label}")
        if rule.get("requiresAnyHeader") and not headers.intersection(rule["requiresAnyHeader"]):
            errors.append(f"boundary {name} requires one of {rule['requiresAnyHeader']}: {label}")
        if rule.get("requiresQuery") and rule["requiresQuery"] not in queries:
            errors.append(f"boundary {name} requires query {rule['requiresQuery']}: {label}")
        if rule.get("requiresDebt"):
            debt = entry.get("debt")
            if debt not in debts:
                errors.append(f"known-debt entry must reference a registered debt: {label}")
    for key in sorted(set(actual) - seen):
        errors.append(f"unregistered unauthenticated route: {key[0]} {key[1]}")
    return errors


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--surface", default=ROOT / "formal/system/compiled-api-surface.json")
    parser.add_argument("--register", default=ROOT / "formal/system/public-routes.json")
    args = parser.parse_args()
    surface = load(args.surface)["surface"]
    errors = check(surface, load(args.register))
    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        return 1
    print("Public route admission checked")
    return 0


if __name__ == "__main__":
    sys.exit(main())

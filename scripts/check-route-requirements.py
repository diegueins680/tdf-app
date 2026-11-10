#!/usr/bin/env python3
"""Map every mutating compiled route to its requirements (SYS-ROUTE-REQUIREMENTS-001).

Compares the POST, PUT, PATCH and DELETE operations in
formal/system/compiled-api-surface.json with the ledger in
formal/system/route-requirements.json. Each operation has exactly one entry,
and each entry has one basis:

  named        the listed requirements name the operation as "METHOD /path"
               (path parameters compare by position, not by name);
  path-named   the listed requirements name the path without a method, and
               the operation is the only mutating method on that path;
  reviewed     a reviewer attributed the operation to the listed requirements;
               the note says why;
  unspecified  no requirement covers the operation: visible debt.

The named bases are recomputed from requirements.json on every check, so a
requirement that starts naming a route moves it out of "unspecified" and a
requirement that stops naming it fails the check. Passing means the ledger is
complete and consistent with the requirement text; it does not prove that a
handler satisfies its requirement.

  --write   regenerate the ledger, keeping reviewed entries
"""

import argparse
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SURFACE = "formal/system/compiled-api-surface.json"
REQUIREMENTS = "formal/system/requirements.json"
LEDGER = "formal/system/route-requirements.json"
MUTATING = ("POST", "PUT", "PATCH", "DELETE")
BASES = ("named", "path-named", "reviewed", "unspecified")
ROUTE_TEXT = re.compile(r"\b(GET|POST|PUT|PATCH|DELETE)\s+(/[A-Za-z0-9_\-./{}:]*)")
PATH_TEXT = re.compile(r"(?<![A-Za-z0-9_])(/[a-z][A-Za-z0-9_\-./{}:]*)")


def load(path):
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


def normalize(path):
    """Compare paths with parameters by position: /a/{id} == /a/{sessionId}."""
    # Prose punctuation after a named path ("POST /a/{id}/cancel: ...") is not part of it.
    path = path.rstrip(".,;):")
    path = re.sub(r"\{[^}/]*\}|(?<=/):[A-Za-z_][A-Za-z0-9_]*", "{}", path)
    return path.rstrip("/") or "/"


def mutating_operations(surface):
    return [(op["method"], op["path"]) for op in surface["operations"] if op["method"] in MUTATING]


def requirement_list(requirements):
    return requirements["requirements"] if isinstance(requirements, dict) else requirements


def named_routes(requirements):
    """(method, normalized path) -> ids, and normalized path -> ids."""
    by_route, by_path = defaultdict(set), defaultdict(set)
    for requirement in requirement_list(requirements):
        text = json.dumps(requirement, ensure_ascii=False)
        for method, path in ROUTE_TEXT.findall(text):
            by_route[(method, normalize(path))].add(requirement["id"])
        # Qualified occurrences establish authority only for their named method.
        # Preserve independent bare-path mentions elsewhere in the requirement.
        for path in PATH_TEXT.findall(ROUTE_TEXT.sub("", text)):
            by_path[normalize(path)].add(requirement["id"])
    return by_route, by_path


def derive(operations, requirements):
    """The basis and requirements the requirement text establishes for each operation."""
    by_route, by_path = named_routes(requirements)
    methods_on_path = defaultdict(set)
    for method, path in operations:
        methods_on_path[normalize(path)].add(method)
    derived = {}
    for method, path in operations:
        key = normalize(path)
        if by_route.get((method, key)):
            derived[(method, path)] = ("named", sorted(by_route[(method, key)]))
        elif by_path.get(key) and len(methods_on_path[key]) == 1:
            derived[(method, path)] = ("path-named", sorted(by_path[key]))
        else:
            derived[(method, path)] = ("unspecified", [])
    return derived


def summarize(routes):
    counts = {basis: 0 for basis in BASES}
    for entry in routes:
        counts[entry["basis"]] += 1
    return {"total": len(routes), **counts, "covered": len(routes) - counts["unspecified"]}


def build(surface, requirements, previous=None):
    operations = mutating_operations(surface)
    derived = derive(operations, requirements)
    reviewed = {}
    for entry in (previous or {}).get("routes", []):
        if entry.get("basis") == "reviewed":
            reviewed[(entry["method"], entry["path"])] = entry
    routes = []
    for method, path in sorted(set(operations), key=lambda item: (item[1], item[0])):
        basis, ids = derived[(method, path)]
        entry = {"method": method, "path": path, "basis": basis, "requirements": ids}
        if basis == "unspecified" and (method, path) in reviewed:
            entry = reviewed[(method, path)]
        routes.append(entry)
    return {
        "schemaVersion": 1,
        "authority": "Generated from the compiled API surface and requirement text by "
                     "scripts/check-route-requirements.py; reviewed entries are kept. "
                     "Coverage here is attribution, not conformance.",
        "summary": summarize(routes),
        "routes": routes,
    }


def check(surface, requirements, ledger):
    errors = []
    operations = mutating_operations(surface)
    derived = derive(operations, requirements)
    known = {requirement["id"] for requirement in requirement_list(requirements)}
    seen = set()
    for entry in ledger.get("routes", []):
        key = (entry.get("method"), entry.get("path"))
        label = f"{key[0]} {key[1]}"
        if key in seen:
            errors.append(f"duplicate ledger entry: {label}")
            continue
        seen.add(key)
        if key not in derived:
            errors.append(f"stale ledger entry (not a mutating compiled route): {label}")
            continue
        basis, ids = entry.get("basis"), entry.get("requirements", [])
        if basis not in BASES:
            errors.append(f"unknown basis {basis!r}: {label}")
            continue
        unknown = [rid for rid in ids if rid not in known]
        if unknown:
            errors.append(f"unknown requirement(s) {unknown}: {label}")
        expected_basis, expected_ids = derived[key]
        if basis == "reviewed":
            if not ids or not str(entry.get("note", "")).strip():
                errors.append(f"reviewed entry needs requirements and a note: {label}")
            if expected_basis != "unspecified":
                errors.append(f"reviewed entry is now {expected_basis} by requirement text; regenerate: {label}")
        elif (basis, sorted(ids)) != (expected_basis, expected_ids):
            errors.append(
                f"{label}: ledger says {basis} {sorted(ids)}, requirement text gives "
                f"{expected_basis} {expected_ids}; regenerate with --write")
    for key in derived:
        if key not in seen:
            errors.append(f"mutating route missing from the ledger: {key[0]} {key[1]}")
    if ledger.get("summary") != summarize([e for e in ledger.get("routes", []) if e.get("basis") in BASES]):
        errors.append("ledger summary does not match its routes; regenerate with --write")
    return errors


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--write", action="store_true", help="regenerate the ledger")
    parser.add_argument("--root", default=str(ROOT))
    args = parser.parse_args(argv)
    root = Path(args.root)
    surface = load(root / SURFACE)["surface"]
    requirements = load(root / REQUIREMENTS)
    ledger_path = root / LEDGER
    if args.write:
        previous = load(ledger_path) if ledger_path.exists() else None
        ledger = build(surface, requirements, previous)
        ledger_path.write_text(json.dumps(ledger, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    else:
        ledger = load(ledger_path)
    errors = check(surface, requirements, ledger)
    for error in errors:
        print(f"error: {error}", file=sys.stderr)
    if errors:
        return 1
    print(f"Route requirements checked: {json.dumps(ledger['summary'])}")
    return 0


if __name__ == "__main__":
    sys.exit(main())

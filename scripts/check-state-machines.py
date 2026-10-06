#!/usr/bin/env python3
"""Check declared lifecycle state machines against the migrated database.

Each declared machine in the domain formal-model.yaml files is bound in
formal/system/state-machine-bindings.json to the table column that stores it.
Against a database with the full production migration manifest applied, the
checker compares the declared state set with the column's effective CHECK
constraint and, where a trigger function encodes the transition relation with
`OLD.col = 'a' AND NEW.col IN (...)` clauses, the declared transitions with the
trigger's. Any difference must equal the reviewed deviation recorded for that
machine exactly, so new drift and silently repaired drift both fail.

Usage:
  check-state-machines.py --database-url URL   # compare with a migrated database
  check-state-machines.py --static             # validate bindings and sources only
"""

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parent.parent
BINDINGS = ROOT / "formal/system/state-machine-bindings.json"


def declared_machines(root=ROOT):
    """Every declared machine, found independently of the bindings."""
    machines = {}
    for path in sorted(root.glob("docs/*/formal-model.yaml")):
        source = path.relative_to(root).as_posix()
        doc = yaml.safe_load(path.read_text()) or {}
        found = dict(doc.get("state_machines", {}))
        if "lifecycle" in doc:
            found[doc["feature"]] = doc["lifecycle"]
        for name, definition in found.items():
            machines[f"{source}#{name}"] = definition
    requirements = json.loads((root / "formal/system/requirements.json").read_text())["requirements"]
    for requirement in requirements:
        state = requirement.get("state")
        if isinstance(state, dict) and "states" in state and "transitions" in state:
            machines[requirement["id"]] = state
    return machines


def states_and_transitions(definition):
    states = set(definition.get("states") or [])
    pairs = set()
    tables = [definition.get("transitions") or {}]
    tables += list((definition.get("method_specific_transitions") or {}).values())
    for table in tables:
        if isinstance(table, dict):
            for source, targets in table.items():
                states.add(source)
                for target in targets or []:
                    states.add(target)
                    pairs.add((source, target))
        elif isinstance(table, list):
            for edge in table:
                if isinstance(edge, dict) and "from" in edge and "to" in edge:
                    sources = edge["from"] if isinstance(edge["from"], list) else [edge["from"]]
                    for source in sources:
                        states.update([source, edge["to"]])
                        pairs.add((source, edge["to"]))
    if definition.get("initial"):
        states.add(definition["initial"])
    return states, pairs


def check_static(bindings, machines):
    errors = []
    for machine_id, binding in bindings["machines"].items():
        if machine_id not in machines:
            errors.append(f"binding for undeclared machine: {machine_id}")
            continue
        for field in ("table", "column", "decision"):
            if not binding.get(field):
                errors.append(f"{machine_id}: binding requires {field}")
    unbound = bindings.get("unbound", {})
    for machine_id in unbound:
        if machine_id not in machines:
            errors.append(f"exclusion for undeclared machine: {machine_id}")
        if machine_id in bindings["machines"]:
            errors.append(f"machine is both bound and excluded: {machine_id}")
    for machine_id in machines:
        if machine_id not in bindings["machines"] and machine_id not in unbound:
            errors.append(f"declared machine has no database binding or reviewed exclusion: {machine_id}")
    return errors


def query(url, sql):
    result = subprocess.run(["psql", url, "-X", "-At", "-v", "ON_ERROR_STOP=1", "-c", sql],
                            capture_output=True, text=True, check=False)
    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip())
    return result.stdout


def effective_states(url, table, column):
    out = query(url, f"""
        SELECT pg_get_constraintdef(c.oid)
        FROM pg_constraint c JOIN pg_attribute a
          ON a.attrelid = c.conrelid AND a.attnum = ANY (c.conkey)
        WHERE c.contype = 'c' AND c.conrelid = 'public.{table}'::regclass
          AND a.attname = '{column}' AND array_length(c.conkey, 1) = 1""")
    sets = [set(re.findall(r"'([^']+)'::text", line)) for line in out.splitlines()
            if "= ANY (ARRAY[" in line]
    if len(sets) != 1:
        raise RuntimeError(f"expected exactly one enumerated CHECK on {table}.{column}, found {len(sets)}")
    return sets[0]


def effective_transitions(url, function, column):
    source = query(url, f"SELECT prosrc FROM pg_proc WHERE oid = 'public.{function}'::regproc")
    pattern = re.compile(
        rf"OLD\.{column}\s*=\s*'(\w+)'\s+AND\s+NEW\.{column}\s+IN\s*\(([^)]*)\)", re.S)
    pairs = set()
    for source_state, targets in pattern.findall(source):
        for target in re.findall(r"'(\w+)'", targets):
            pairs.add((source_state, target))
    if not pairs:
        raise RuntimeError(f"no transition clauses for {column} found in {function}")
    return pairs


def as_sorted(values):
    return sorted([list(v) if isinstance(v, tuple) else v for v in values])


def compare(machine_id, binding, definition, url):
    errors = []
    states, pairs = states_and_transitions(definition)
    expected = binding.get("deviation", {})
    actual_states = effective_states(url, binding["table"], binding["column"])
    observed = {
        "statesOnlyDeclared": as_sorted(states - actual_states),
        "statesOnlyImplemented": as_sorted(actual_states - states),
    }
    if binding.get("transitionFunction"):
        actual_pairs = effective_transitions(url, binding["transitionFunction"], binding["column"])
        observed["transitionsOnlyDeclared"] = as_sorted(pairs - actual_pairs)
        observed["transitionsOnlyImplemented"] = as_sorted(actual_pairs - pairs)
    for key, value in observed.items():
        if value != as_sorted(tuple(v) if isinstance(v, list) else v for v in expected.get(key, [])):
            errors.append(f"{machine_id}: {key} is {value}, reviewed deviation is {expected.get(key, [])}")
    for key in expected:
        if key != "classification" and key not in observed:
            errors.append(f"{machine_id}: reviewed deviation {key} has no corresponding check")
    return errors


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--database-url")
    mode.add_argument("--static", action="store_true")
    args = parser.parse_args()
    bindings = json.loads(BINDINGS.read_text())
    machines = declared_machines()
    errors = check_static(bindings, machines)
    checked = 0
    if args.database_url and not errors:
        for machine_id, binding in bindings["machines"].items():
            try:
                errors += compare(machine_id, binding, machines[machine_id], args.database_url)
                checked += 1
            except RuntimeError as failure:
                errors.append(f"{machine_id}: {failure}")
    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        return 1
    if args.database_url:
        print(f"State machine correspondence checked for {checked} machines")
    else:
        print("State machine bindings checked")
    return 0


if __name__ == "__main__":
    sys.exit(main())

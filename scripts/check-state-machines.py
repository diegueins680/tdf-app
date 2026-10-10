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

A machine may also bind the backend validator that enforces its transitions
(`code`: Haskell file, validator and the renderer that maps constructors to
stored state names). Both modes compare the validator's `allowedTransitions`
list with the declared transitions under the same exact-deviation rule
(`code.deviation`).

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


def without_haskell_comments(text):
    """Commented-out code is not implemented: drop line comments and nested block comments.

    String literals are copied through so that comment markers inside them stay text.
    """
    kept, depth, index = [], 0, 0
    while index < len(text):
        pair = text[index:index + 2]
        if pair == "{-":
            depth, index = depth + 1, index + 2
        elif depth and pair == "-}":
            depth, index = depth - 1, index + 2
        elif depth:
            if text[index] == "\n":
                kept.append("\n")
            index += 1
        elif pair == "--":
            index = text.find("\n", index) if "\n" in text[index:] else len(text)
        elif text[index] == '"':
            end = index + 1
            while end < len(text) and text[end] not in '"\n':
                end += 2 if text[end] == "\\" else 1
            kept.append(text[index:end + 1])
            index = end + 1
        else:
            kept.append(text[index])
            index += 1
    if depth:
        raise RuntimeError("unterminated Haskell block comment")
    return "".join(kept)


def declaration_body(source, start):
    """Text of the top-level declaration that begins at start, up to the next one."""
    following = re.compile(r"^\S", re.M).search(source, source.index("\n", start) + 1)
    return source[start:following.start() if following else len(source)]


def code_transitions(spec, root=ROOT):
    """Pairs in the validator's allowedTransitions list, as stored state names."""
    # Comments go first so that neither a commented mapping, a commented edge nor a
    # bracket inside a comment can stand in for live code.
    source = without_haskell_comments((root / spec["file"]).read_text())
    renderer = re.search(rf"^{re.escape(spec['renderer'])} (\w+) = case \1 of\n", source, re.M)
    if not renderer:
        raise RuntimeError(f"renderer {spec['renderer']} not found in {spec['file']}")
    mappings = re.findall(r'^\s+([A-Z]\w*)\s*->\s*"(\w+)"', declaration_body(source, renderer.start()), re.M)
    names = dict(mappings)
    if len(names) != len(mappings):
        raise RuntimeError(f"{spec['renderer']} maps a constructor more than once")
    definition = re.search(rf"^{re.escape(spec['validator'])} (?![^\n]*::)\w", source, re.M)
    if not definition:
        raise RuntimeError(f"validator {spec['validator']} not found in {spec['file']}")
    # Only this validator's own clause: a later function's table must not be read instead.
    body = declaration_body(source, definition.start())
    table = re.search(r"allowedTransitions\s*=\s*\[(.*?)\]", body, re.S)
    if not table:
        raise RuntimeError(f"{spec['validator']} has no allowedTransitions list")
    # The whole right-hand side must be the literal list: `[...] ++ more` would hide edges.
    following = body[table.end():].lstrip()
    if following and not re.match(r"[A-Za-z_]\w*'*(\s+[\w']+)*\s*(=|::|\|)", following):
        raise RuntimeError(f"{spec['validator']} allowedTransitions is not a single literal list")
    listed = table.group(1)
    pairs = set()
    for source_state, target in re.findall(r"\((\w+),\s*(\w+)\)", listed):
        for constructor in (source_state, target):
            if constructor not in names:
                raise RuntimeError(f"{constructor} has no stored name in {spec['renderer']}")
        pairs.add((names[source_state], names[target]))
    if not pairs:
        raise RuntimeError(f"{spec['validator']} allowedTransitions is empty")
    return pairs


def compare_code(machine_id, spec, definition, root=ROOT):
    errors = []
    _, pairs = states_and_transitions(definition)
    implemented = code_transitions(spec, root)
    expected = spec.get("deviation", {})
    observed = {
        "transitionsOnlyDeclared": as_sorted(pairs - implemented),
        "transitionsOnlyImplemented": as_sorted(implemented - pairs),
    }
    for key, value in observed.items():
        if value != as_sorted(tuple(v) for v in expected.get(key, [])):
            errors.append(f"{machine_id}: code {key} is {value}, reviewed deviation is {expected.get(key, [])}")
    for key in expected:
        if key != "classification" and key not in observed:
            errors.append(f"{machine_id}: reviewed code deviation {key} has no corresponding check")
    return errors


# Validators reviewed under AUTHORITY-053. A binding that loses its `code` object must fail,
# not quietly reduce the number of validators compared.
REQUIRED_CODE_BINDINGS = (
    "docs/revenue-platform/formal-model.yaml#event_ticket_fulfillment",
    "docs/revenue-platform/formal-model.yaml#quote",
    "docs/revenue-platform/formal-model.yaml#service_booking_fulfillment",
    "docs/revenue-platform/formal-model.yaml#marketplace_rental_fulfillment",
)


def check_code(bindings, machines, root=ROOT):
    errors = []
    for machine_id in REQUIRED_CODE_BINDINGS:
        if not bindings["machines"].get(machine_id, {}).get("code"):
            errors.append(f"{machine_id}: required backend validator binding is missing")
    for machine_id, binding in bindings["machines"].items():
        spec = binding.get("code")
        if spec is None or machine_id not in machines:
            continue
        missing = [f for f in ("file", "validator", "renderer") if not spec.get(f)]
        if missing:
            errors.append(f"{machine_id}: code binding requires {', '.join(missing)}")
            continue
        try:
            errors += compare_code(machine_id, spec, machines[machine_id], root)
        except (OSError, RuntimeError) as failure:
            errors.append(f"{machine_id}: {failure}")
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
    errors += check_code(bindings, machines)
    coded = sum(1 for binding in bindings["machines"].values() if binding.get("code"))
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
        print(f"State machine correspondence checked for {checked} machines"
              f" ({coded} also against backend validators)")
    else:
        print(f"State machine bindings checked; {coded} backend validators match their models")
    return 0


if __name__ == "__main__":
    sys.exit(main())

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
stored state names). The validator holds a literal `allowedTransitions` pair
list, or (`targets` instead of `validator`) one clause per source constructor
returning the literal list of admitted targets; the renderer is a `case` or
one clause per constructor, in `rendererFile` when it lives elsewhere. Both
modes compare the implemented transitions with the declared ones under the
same exact-deviation rule (`code.deviation`). A third shape (`relation`) is a
membership predicate whose `where` binds `transitions = Set.fromList [ ... ]`
of literal pairs; its renderer is a parse table (`name value = lookup value
[("stored", Constructor), ...]`) or `identity` when the pairs are string
literals.

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


def stored_names(spec, root=ROOT):
    """Constructor -> stored state name from the renderer: a `case` with literal
    branches, one literal clause per constructor, or a parse table
    `name value = lookup value [("stored", Constructor), ...]`."""
    if spec["renderer"] == "identity":
        return None
    renderer_file = spec.get("rendererFile", spec["file"])
    source = without_haskell_comments((root / renderer_file).read_text())
    name = re.escape(spec["renderer"])
    mappings = []
    renderer = re.search(rf"^{name}[ \t]+(\w+) = case \1 of\n", source, re.M)
    table = re.search(rf"^{name}[ \t]+(\w+) = lookup \1\s*\[", source, re.M)
    if table:
        # One clause only: an earlier specific clause would parse a stored name
        # to another constructor before the table is consulted.
        clauses = [c for c in re.finditer(rf"^{name}[ \t]+(.*)$", source, re.M) if not c.group(1).startswith("::")]
        if len(clauses) != 1:
            raise RuntimeError(f"{spec['renderer']} must be a single parse-table clause")
        body = declaration_body(source, table.start())
        listed = body[body.index("[") + 1:]
        if "]" not in listed:
            raise RuntimeError(f"{spec['renderer']} parse table is not closed")
        listed, following = listed.split("]", 1)
        if following.strip():
            raise RuntimeError(f"{spec['renderer']} parse table is not the whole definition")
        if re.sub(r'\("(\w+)",\s*([A-Z]\w*)\)', "", listed).replace(",", "").strip():
            raise RuntimeError(f"{spec['renderer']} parse table has an entry that is not a literal pair")
        mappings = [(constructor, stored) for stored, constructor in re.findall(r'\("(\w+)",\s*([A-Z]\w*)\)', listed)]
        if len({stored for _, stored in mappings}) != len(mappings):
            raise RuntimeError(f"{spec['renderer']} maps a stored name more than once")
    elif renderer:
        # Every branch must be exactly `Constructor -> "name"`: a prefix match would
        # read `"lost" <> "_archived"` as "lost".
        for branch in declaration_body(source, renderer.start()).split("\n")[1:]:
            if not branch.strip():
                continue
            literal = re.fullmatch(r'\s+([A-Z]\w*)\s*->\s*"(\w+)"\s*', branch)
            if not literal:
                raise RuntimeError(f"{spec['renderer']} has a branch that is not a literal name: {branch.strip()}")
            mappings.append(literal.groups())
    else:
        # Clause form: every clause must be exactly `renderer Constructor = "name"`.
        for clause in re.finditer(rf"^{name}[ \t]+(.*)$", source, re.M):
            if clause.group(1).startswith("::"):
                continue
            literal = re.fullmatch(r'([A-Z]\w*)\s*=\s*"(\w+)"\s*', clause.group(1))
            if not literal:
                raise RuntimeError(f"{spec['renderer']} has a clause that is not a literal name: {clause.group(0).strip()}")
            mappings.append(literal.groups())
        if not mappings:
            raise RuntimeError(f"renderer {spec['renderer']} not found in {renderer_file}")
    names = dict(mappings)
    if len(names) != len(mappings):
        raise RuntimeError(f"{spec['renderer']} maps a constructor more than once")
    return names


def target_transitions(spec, source, names):
    """Pairs from a targets function: one clause `targets Constructor = [C, ...]`
    per constructor the renderer names, each a literal constructor list."""
    name = re.escape(spec["targets"])
    clauses = {}
    for clause in re.finditer(rf"^{name}[ \t]+(.*)$", source, re.M):
        if clause.group(1).startswith("::"):
            continue
        literal = re.fullmatch(r"([A-Z]\w*)\s*=\s*\[([^\]]*)\]\s*", clause.group(1))
        if not literal:
            raise RuntimeError(f"{spec['targets']} has a clause that is not a literal target list: {clause.group(0).strip()}")
        constructor, listed = literal.groups()
        # The clause must end on its line: an indented continuation (`<> more`,
        # `++ reopenTargets`) would add edges this reader cannot see.
        if declaration_body(source, clause.start())[clause.end() - clause.start():].strip():
            raise RuntimeError(f"{spec['targets']} clause for {constructor} does not end on its line")
        if constructor in clauses:
            raise RuntimeError(f"{spec['targets']} has more than one clause for {constructor}")
        if re.sub(r"[A-Z]\w*", "", listed).replace(",", "").strip():
            raise RuntimeError(f"{spec['targets']} clause for {constructor} has an entry that is not a constructor")
        clauses[constructor] = re.findall(r"[A-Z]\w*", listed)
    if not clauses:
        raise RuntimeError(f"targets {spec['targets']} not found in {spec['file']}")
    # A constructor without its own clause would fall to a wildcard or a crash;
    # either way its edges would be invisible here.
    for constructor in names:
        if constructor not in clauses:
            raise RuntimeError(f"{spec['targets']} has no clause for {constructor}")
    pairs = set()
    for constructor, targets in clauses.items():
        for state in [constructor] + targets:
            if state not in names:
                raise RuntimeError(f"{state} has no stored name in {spec['renderer']}")
        pairs.update((names[constructor], names[target]) for target in targets)
    if not pairs:
        raise RuntimeError(f"{spec['targets']} admits no transition")
    return pairs


def relation_transitions(spec, source, names):
    """Pairs of a membership predicate. The predicate's single clause must be
    exactly `a == b || (a, b) `Set.member` transitions` (or, for string-state
    relations, `(a == b && a `Set.member` states) || (a, b) `Set.member`
    transitions`), and its `where` must bind `transitions = Set.fromList [ ... ]`
    once, as the only list, of literal pairs. Identity transitions are not read."""
    name = re.escape(spec["relation"])
    clauses = [c for c in re.finditer(rf"^{name}[ \t]+(.*)$", source, re.M) if not c.group(1).startswith("::")]
    if not clauses:
        raise RuntimeError(f"relation {spec['relation']} not found in {spec['file']}")
    if len(clauses) != 1:
        raise RuntimeError(f"{spec['relation']} must be a single clause")
    body = declaration_body(source, clauses[0].start())
    head, separator, where = body.partition("\nwhere\n" if re.search(r"^where", body, re.M) else "\n  where\n")
    if not separator:
        raise RuntimeError(f"{spec['relation']} has no where clause binding transitions")
    clause = re.fullmatch(rf"{name}[ \t]+(\w+)[ \t]+(\w+)[ \t]*=\s*(.*)", head.strip(), re.S)
    if not clause:
        raise RuntimeError(f"{spec['relation']} is not a two-argument clause")
    a, b, decision = clause.group(1), clause.group(2), " ".join(clause.group(3).split())
    admitted = [f"{a} == {b} || ({a}, {b}) `Set.member` transitions",
                f"({a} == {b} && {a} `Set.member` states) || ({a}, {b}) `Set.member` transitions"]
    if decision not in admitted:
        raise RuntimeError(f"{spec['relation']} does not decide membership in transitions: {decision}")
    # Haskell layout fixes the where-block's declaration column at its first
    # lexeme. A nested let/where can shadow `transitions`; its table must never
    # stand in for the binding actually used by the membership predicate.
    where = where.expandtabs(8)
    first = re.search(r"^( *)(\S)", where, re.M)
    if first is None or not first.group(1):
        raise RuntimeError(f"{spec['relation']} must bind transitions = Set.fromList once")
    indent = len(first.group(1))
    starts = list(re.finditer(rf"^ {{{indent}}}\S", where, re.M))
    blocks = [where[start.start():starts[i + 1].start() if i + 1 < len(starts) else len(where)]
              for i, start in enumerate(starts)]
    bindings = [block for block in blocks if re.match(r"transitions(?:\s|=)", block.lstrip())]
    table = re.match(r"\s*transitions\s*=\s*Set\.fromList\s*\[(.*?)\]", bindings[0], re.S) if len(bindings) == 1 else None
    if table is None:
        raise RuntimeError(f"{spec['relation']} must bind transitions = Set.fromList once")
    if bindings[0][table.end():].strip():
        raise RuntimeError(f"{spec['relation']} transitions is not a single literal set")
    listed = table.group(1)
    if names is None:
        pattern = r'\(\s*"(\w+)"\s*,\s*"(\w+)"\s*\)'
    else:
        pattern = r"\(\s*([A-Z]\w*)\s*,\s*([A-Z]\w*)\s*\)"
    if re.sub(pattern, "", listed).replace(",", "").strip():
        raise RuntimeError(f"{spec['relation']} transitions has an entry that is not a literal pair")
    pairs = set()
    for source_state, target in re.findall(pattern, listed):
        if names is not None:
            for constructor in (source_state, target):
                if constructor not in names:
                    raise RuntimeError(f"{constructor} has no stored name in {spec['renderer']}")
            pairs.add((names[source_state], names[target]))
        else:
            pairs.add((source_state, target))
    if not pairs:
        raise RuntimeError(f"{spec['relation']} transitions is empty")
    return pairs


def code_transitions(spec, root=ROOT):
    """Implemented transition pairs of the bound code, as stored state names."""
    # Comments go first so that neither a commented mapping, a commented edge nor a
    # bracket inside a comment can stand in for live code.
    source = without_haskell_comments((root / spec["file"]).read_text())
    names = stored_names(spec, root)
    if spec.get("relation"):
        return relation_transitions(spec, source, names)
    if names is None:
        raise RuntimeError(f"{spec['renderer']} renderer is only admissible with a relation of string literals")
    if spec.get("targets"):
        return target_transitions(spec, source, names)
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
    # Every element must be a literal pair: a named entry would add an edge this reader cannot see.
    if re.sub(r"\(\w+,\s*\w+\)", "", listed).replace(",", "").strip():
        raise RuntimeError(f"{spec['validator']} allowedTransitions has an entry that is not a literal pair")
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
    # Targets-function binding reviewed under AUTHORITY-080.
    "docs/operations-control-center/formal-model.yaml#operations.control-center",
    # Membership-relation bindings reviewed under AUTHORITY-081.
    "docs/music-directory/formal-model.yaml#profile",
    "docs/music-directory/formal-model.yaml#claim",
    "docs/music-directory/formal-model.yaml#classified",
    "docs/music-directory/formal-model.yaml#application",
    "docs/music-directory/formal-model.yaml#invitation",
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
        missing = [f for f in ("file", "renderer") if not spec.get(f)]
        if sum(bool(spec.get(kind)) for kind in ("validator", "targets", "relation")) != 1:
            missing.append("exactly one of validator, targets, relation")
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

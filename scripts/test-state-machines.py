#!/usr/bin/env python3
"""Negative controls for scripts/check-state-machines.py.

Backend validator controls need no database:
  test-state-machines.py --static
Database controls require the production migration manifest applied:
  test-state-machines.py --database-url URL
A disposable table is created and dropped to exercise database-side drift.
"""

import argparse
import copy
import importlib.util
import json
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location("check_state_machines", ROOT / "scripts/check-state-machines.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)

QUOTE = "docs/revenue-platform/formal-model.yaml#quote"
CLAIM = "docs/music-directory/formal-model.yaml#claim"
DISTRIBUTION = "docs/revenue-platform/formal-model.yaml#distribution"
RENTAL = "docs/revenue-platform/formal-model.yaml#marketplace_rental_fulfillment"
OPERATIONS = "docs/operations-control-center/formal-model.yaml#operations.control-center"


def expect(name, errors, fragment):
    if not any(fragment in error for error in errors):
        raise AssertionError(f"{name}: expected an error containing {fragment!r}, got {errors}")


def code_controls(bindings, machines):
    assert gate.check_code(bindings, machines) == []
    spec = bindings["machines"][RENTAL]["code"]

    rental = copy.deepcopy(machines[RENTAL])
    rental["transitions"]["deposit_refund_due"] = ["closed"]
    expect("validator edge absent from model", gate.compare_code(RENTAL, spec, rental),
           "code transitionsOnlyImplemented is [['deposit_refund_due', 'disputed']]")

    rental = copy.deepcopy(machines[RENTAL])
    rental["transitions"]["closed"] = ["on_hold"]
    expect("model edge absent from validator", gate.compare_code(RENTAL, spec, rental),
           "code transitionsOnlyDeclared is [['closed', 'on_hold']]")

    repaired = dict(spec, deviation={"transitionsOnlyImplemented": [["lost", "closed"]]})
    expect("stale code deviation", gate.compare_code(RENTAL, repaired, machines[RENTAL]),
           "code transitionsOnlyImplemented is []")

    partial = copy.deepcopy(bindings)
    del partial["machines"][RENTAL]["code"]["renderer"]
    expect("incomplete code binding", gate.check_code(partial, machines), "code binding requires renderer")

    renamed = copy.deepcopy(bindings)
    renamed["machines"][RENTAL]["code"]["validator"] = "validateRetiredRentalTransition"
    expect("validator not found", gate.check_code(renamed, machines), "validator validateRetiredRentalTransition")

    with tempfile.TemporaryDirectory() as scratch:
        root = Path(scratch)
        target = root / spec["file"]
        target.parent.mkdir(parents=True)
        source = (gate.ROOT / spec["file"]).read_text()
        target.write_text(source.replace("(RentalLost, RentalDisputed)", "(RentalLost, RentalWrittenOff)", 1))
        expect("constructor without stored name", gate.check_code(bindings, machines, root),
               "RentalWrittenOff has no stored name")
        target.write_text(source.replace("      , (RentalLost, RentalDisputed)\n", "", 1))
        expect("edge removed from validator source", gate.check_code(bindings, machines, root),
               "code transitionsOnlyDeclared is [['lost', 'disputed']]")
        target.write_text(source.replace("      , (RentalLost, RentalDisputed)\n",
                                         "      -- , (RentalLost, RentalDisputed)\n", 1))
        expect("edge commented out with a line comment", gate.check_code(bindings, machines, root),
               "code transitionsOnlyDeclared is [['lost', 'disputed']]")
        target.write_text(source.replace("      , (RentalLost, RentalDisputed)\n",
                                         "      {- , (RentalLost, RentalDisputed)\n      -}\n", 1))
        expect("edge commented out with a block comment", gate.check_code(bindings, machines, root),
               "code transitionsOnlyDeclared is [['lost', 'disputed']]")
        target.write_text(source.replace(
            '  RentalLost -> "lost"\n',
            '  RentalLost -> "renamed"\n  {- previously\n  RentalLost -> "lost"\n  -}\n', 1))
        expect("renderer mapping kept only in a block comment", gate.check_code(bindings, machines, root),
               "'renamed'")
        target.write_text(source.replace(
            '  RentalLost -> "lost"\n', '  RentalLost -> "renamed"\n  RentalLost -> "lost"\n', 1))
        expect("renderer maps a constructor twice", gate.check_code(bindings, machines, root),
               "maps a constructor more than once")
        target.write_text(source.replace(
            "      , (RentalLost, RentalDisputed)\n",
            "      -- old table: [(RentalLost, RentalDisputed)]\n      , (RentalLost, RentalDisputed)\n"
            "      , (RentalClosed, RentalOnHold)\n", 1))
        expect("edge listed after a bracket inside a comment", gate.check_code(bindings, machines, root),
               "code transitionsOnlyImplemented is [['closed', 'on_hold']]")
        renamed_table = source.replace("    allowedTransitions =\n      [ (RentalOnHold, RentalConfirmed)",
                                       "    retiredTransitions =\n      [ (RentalOnHold, RentalConfirmed)", 1)
        assert renamed_table != source
        target.write_text(renamed_table + "\nlaterTable :: [(Int, Int)]\nlaterTable = allowedTransitions\n"
                          "  where\n    allowedTransitions = [(RentalOnHold, RentalConfirmed)]\n")
        expect("table taken from a later declaration", gate.check_code(bindings, machines, root),
               "has no allowedTransitions list")
        target.write_text(source.replace(
            "      , (RentalLost, RentalDisputed)\n",
            "      {- outer {- inner -} , (RentalLost, RentalDisputed)\n      -}\n", 1))
        expect("edge inside a nested block comment", gate.check_code(bindings, machines, root),
               "code transitionsOnlyDeclared is [['lost', 'disputed']]")
        extended = source.replace("      , (RentalDisputed, RentalClosed)\n      ]",
                                  "      , (RentalDisputed, RentalClosed)\n      ] ++ additionalTransitions", 1)
        assert extended != source
        target.write_text(extended)
        expect("list extended by another expression", gate.check_code(bindings, machines, root),
               "is not a single literal list")
        named = source.replace("      , (RentalLost, RentalDisputed)\n",
                               "      , (RentalLost, RentalDisputed)\n      , hiddenTransition\n", 1)
        assert named != source
        target.write_text(named)
        expect("named entry inside the list", gate.check_code(bindings, machines, root),
               "has an entry that is not a literal pair")
        computed = source.replace('  RentalLost -> "lost"\n', '  RentalLost -> "lost" <> "_archived"\n', 1)
        assert computed != source
        target.write_text(computed)
        expect("renderer branch computed from a literal", gate.check_code(bindings, machines, root),
               "has a branch that is not a literal name")
    assert gate.without_haskell_comments('x = "-- {- text" -- gone\ny') == 'x = "-- {- text" \ny'

    unbound = copy.deepcopy(bindings)
    targets_negative = targets_controls(bindings, machines)
    del unbound["machines"][RENTAL]["code"]
    expect("required validator binding removed", gate.check_code(unbound, machines),
           "required backend validator binding is missing")
    return 18 + targets_negative



def targets_controls(bindings, machines):
    """The operations work-item binding: a targets function and a clause renderer."""
    spec = bindings["machines"][OPERATIONS]["code"]
    operations = machines[OPERATIONS]
    assert gate.compare_code(OPERATIONS, spec, operations) == []
    both = copy.deepcopy(bindings)
    both["machines"][OPERATIONS]["code"]["validator"] = "validateTransition"
    expect("validator and targets together", gate.check_code(both, machines), "exactly one of validator, targets")
    renamed = copy.deepcopy(bindings)
    renamed["machines"][OPERATIONS]["code"]["targets"] = "retiredTargets"
    expect("targets not found", gate.check_code(renamed, machines), "targets retiredTargets not found")
    with tempfile.TemporaryDirectory() as scratch:
        root = Path(scratch)
        for relative in (spec["file"], spec["rendererFile"]):
            (root / relative).parent.mkdir(parents=True, exist_ok=True)
            (root / relative).write_text((gate.ROOT / relative).read_text())
        model = (gate.ROOT / spec["file"]).read_text()
        types = (gate.ROOT / spec["rendererFile"]).read_text()
        target, renderer = root / spec["file"], root / spec["rendererFile"]
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets WorkArchived = []", 1))
        expect("edge removed from a targets clause", gate.check_code(bindings, machines, root),
               "code transitionsOnlyDeclared is [['archived', 'in_progress']]")
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets WorkArchived = [WorkInProgress, WorkNew]", 1))
        expect("edge added to a targets clause", gate.check_code(bindings, machines, root),
               "code transitionsOnlyImplemented is [['archived', 'new']]")
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]\n", "", 1))
        expect("constructor without a targets clause", gate.check_code(bindings, machines, root),
               "allowedTargets has no clause for WorkArchived")
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets WorkArchived = [WorkInProgress]\nallowedTargets WorkArchived = [WorkNew]", 1))
        expect("constructor with two targets clauses", gate.check_code(bindings, machines, root),
               "allowedTargets has more than one clause for WorkArchived")
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets WorkArchived = reopenTargets", 1))
        expect("targets clause that is not a literal list", gate.check_code(bindings, machines, root),
               "allowedTargets has a clause that is not a literal target list")
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets WorkArchived = [WorkInProgress, WorkRetired]", 1))
        expect("target constructor without stored name", gate.check_code(bindings, machines, root),
               "WorkRetired has no stored name")
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets WorkArchived = [] -- [WorkInProgress]", 1))
        expect("edge kept only in a comment", gate.check_code(bindings, machines, root),
               "code transitionsOnlyDeclared is [['archived', 'in_progress']]")
        target.write_text(model)
        renderer.write_text(types.replace('workStatusText WorkArchived = "archived"', 'workStatusText WorkArchived = "archived"\nworkStatusText WorkArchived = "closed"', 1))
        expect("renderer clause repeated", gate.check_code(bindings, machines, root), "workStatusText maps a constructor more than once")
        renderer.write_text(types.replace('workStatusText WorkArchived = "archived"', 'workStatusText WorkArchived = archivedName', 1))
        expect("renderer clause that is not a literal", gate.check_code(bindings, machines, root),
               "workStatusText has a clause that is not a literal name")
        renderer.write_text(types.replace('workStatusText WorkArchived = "archived"\n', '', 1))
        expect("renderer clause removed", gate.check_code(bindings, machines, root), "WorkArchived has no stored name")
    unbound = copy.deepcopy(bindings)
    del unbound["machines"][OPERATIONS]["code"]
    expect("required targets binding removed", gate.check_code(unbound, machines),
           f"{OPERATIONS}: required backend validator binding is missing")
    return 13

def main():
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--database-url")
    mode.add_argument("--static", action="store_true")
    args = parser.parse_args()
    bindings = json.loads(gate.BINDINGS.read_text())
    machines = gate.declared_machines()

    assert gate.check_static(bindings, machines) == []
    code_negative = code_controls(bindings, machines)
    if args.static:
        print(f"Backend validator controls passed ({code_negative} negative)")
        return 0
    url = args.database_url
    for machine_id, binding in bindings["machines"].items():
        assert gate.compare(machine_id, binding, machines[machine_id], url) == [], machine_id

    claim = copy.deepcopy(machines[CLAIM])
    claim["transitions"]["draft"] = [s for s in claim["transitions"]["draft"] if s != "withdrawn"]
    claim["transitions"].pop("withdrawn", None)
    for targets in claim["transitions"].values():
        if "withdrawn" in targets:
            targets.remove("withdrawn")
    expect("database state absent from model", gate.compare(CLAIM, bindings["machines"][CLAIM], claim, url),
           "statesOnlyImplemented is ['withdrawn']")

    quote = copy.deepcopy(machines[QUOTE])
    quote["transitions"]["completed"] = ["refunded"]
    errors = gate.compare(QUOTE, bindings["machines"][QUOTE], quote, url)
    expect("model state rejected by CHECK", errors, "statesOnlyDeclared is ['refunded']")
    expect("model transition absent from trigger", errors, "transitionsOnlyDeclared")

    quote = copy.deepcopy(machines[QUOTE])
    quote["transitions"]["accepted"] = ["deposit_due", "cancelled"]
    expect("trigger transition absent from model", gate.compare(QUOTE, bindings["machines"][QUOTE], quote, url),
           "transitionsOnlyImplemented is [['accepted', 'expired']]")

    stale = copy.deepcopy(bindings["machines"][DISTRIBUTION])
    stale["deviation"]["statesOnlyDeclared"] = stale["deviation"]["statesOnlyDeclared"][1:]
    expect("deviation no longer exact", gate.compare(DISTRIBUTION, stale, machines[DISTRIBUTION], url),
           "reviewed deviation")

    repaired = copy.deepcopy(bindings["machines"][CLAIM])
    repaired["deviation"] = {"statesOnlyDeclared": ["withdrawn"]}
    expect("stale deviation after repair", gate.compare(CLAIM, repaired, machines[CLAIM], url),
           "statesOnlyDeclared is []")

    unbound = copy.deepcopy(bindings)
    del unbound["machines"][CLAIM]
    expect("declared machine without binding", gate.check_static(unbound, machines), "has no database binding")

    orphan = copy.deepcopy(bindings)
    orphan["machines"]["docs/revenue-platform/formal-model.yaml#retired"] = {"table": "t", "column": "c", "decision": "x"}
    expect("binding without declaration", gate.check_static(orphan, machines), "binding for undeclared machine")

    unsourced = copy.deepcopy(bindings)
    unsourced["machines"] = {k: v for k, v in unsourced["machines"].items()
                             if not k.startswith("docs/music-directory/")}
    expect("every binding for one source removed", gate.check_static(unsourced, machines),
           "has no database binding")

    stale_exclusion = copy.deepcopy(bindings)
    stale_exclusion["unbound"]["PAY-RETIRED-001"] = "retired"
    expect("exclusion for undeclared machine", gate.check_static(stale_exclusion, machines),
           "exclusion for undeclared machine")

    double = copy.deepcopy(bindings)
    double["unbound"][CLAIM] = "also excluded"
    expect("bound and excluded", gate.check_static(double, machines), "both bound and excluded")

    gate.query(url, "DROP TABLE IF EXISTS public.state_machine_negative_control;"
                    " CREATE TABLE public.state_machine_negative_control"
                    " (status text CHECK (status IN ('draft','submitted','under_review','approved')))")
    try:
        drifted = {"table": "state_machine_negative_control", "column": "status", "decision": "control"}
        expect("database dropped a declared state",
               gate.compare(CLAIM, drifted, machines[CLAIM], url), "statesOnlyDeclared is ['more_evidence_requested'")
    finally:
        gate.query(url, "DROP TABLE IF EXISTS public.state_machine_negative_control")

    print(f"State machine correspondence controls passed (17 positive, {12 + code_negative} negative)")
    return 0


if __name__ == "__main__":
    sys.exit(main())

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
    return 7


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

#!/usr/bin/env python3
"""Negative controls for scripts/check-state-machines.py.

Requires a database with the production migration manifest applied:
  test-state-machines.py --database-url URL
A disposable table is created and dropped to exercise database-side drift.
"""

import argparse
import copy
import importlib.util
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location("check_state_machines", ROOT / "scripts/check-state-machines.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)

QUOTE = "docs/revenue-platform/formal-model.yaml#quote"
CLAIM = "docs/music-directory/formal-model.yaml#claim"
DISTRIBUTION = "docs/revenue-platform/formal-model.yaml#distribution"


def expect(name, errors, fragment):
    if not any(fragment in error for error in errors):
        raise AssertionError(f"{name}: expected an error containing {fragment!r}, got {errors}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--database-url", required=True)
    url = parser.parse_args().database_url
    bindings = json.loads(gate.BINDINGS.read_text())
    machines = gate.declared_machines()

    assert gate.check_static(bindings, machines) == []
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

    gate.query(url, "DROP TABLE IF EXISTS public.state_machine_negative_control;"
                    " CREATE TABLE public.state_machine_negative_control"
                    " (status text CHECK (status IN ('draft','submitted','under_review','approved')))")
    try:
        drifted = {"table": "state_machine_negative_control", "column": "status", "decision": "control"}
        expect("database dropped a declared state",
               gate.compare(CLAIM, drifted, machines[CLAIM], url), "statesOnlyDeclared is ['more_evidence_requested'")
    finally:
        gate.query(url, "DROP TABLE IF EXISTS public.state_machine_negative_control")

    print("State machine correspondence controls passed (17 positive, 9 negative)")
    return 0


if __name__ == "__main__":
    sys.exit(main())

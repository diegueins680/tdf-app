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
PROFILE = "docs/music-directory/formal-model.yaml#profile"


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
    del unbound["machines"][RENTAL]["code"]
    expect("required validator binding removed", gate.check_code(unbound, machines),
           "required backend validator binding is missing")
    return 18 + targets_controls(bindings, machines) + relation_controls(bindings, machines)



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
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets WorkArchived = [WorkInProgress]\n  <> [WorkNew]", 1))
        expect("clause continued on the next line", gate.check_code(bindings, machines, root),
               "allowedTargets clause for WorkArchived does not end on its line")
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets\tWorkArchived = [WorkInProgress, WorkNew]\nallowedTargets WorkArchived = [WorkInProgress]", 1))
        expect("tab-separated clause before the visible one", gate.check_code(bindings, machines, root),
               "allowedTargets has more than one clause for WorkArchived")
        target.write_text(model.replace("allowedTargets WorkArchived = [WorkInProgress]", "allowedTargets WorkArchived = [WorkInProgress]\nallowedTargets _ = []", 1))
        expect("wildcard clause", gate.check_code(bindings, machines, root),
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
    return 16


def relation_controls(bindings, machines):
    """The directory profile and claim bindings: a membership relation with a
    parse-table renderer, and one with string-literal pairs."""
    spec = bindings["machines"][PROFILE]["code"]
    assert gate.compare_code(PROFILE, spec, machines[PROFILE]) == []
    assert gate.compare_code(CLAIM, bindings["machines"][CLAIM]["code"], machines[CLAIM]) == []
    renamed = copy.deepcopy(bindings)
    renamed["machines"][PROFILE]["code"]["relation"] = "retiredProfileTransition"
    expect("relation not found", gate.check_code(renamed, machines), "relation retiredProfileTransition not found")
    identity = copy.deepcopy(bindings)
    identity["machines"][PROFILE]["code"]["renderer"] = "identity"
    expect("identity renderer on constructor pairs", gate.check_code(identity, machines),
           "allowedProfileTransition transitions has an entry that is not a literal pair")
    with tempfile.TemporaryDirectory() as scratch:
        root = Path(scratch)
        for relative in (spec["file"], spec["rendererFile"]):
            (root / relative).parent.mkdir(parents=True, exist_ok=True)
            (root / relative).write_text((gate.ROOT / relative).read_text())
        policy = (gate.ROOT / spec["file"]).read_text()
        server = (gate.ROOT / spec["rendererFile"]).read_text()
        target, renderer = root / spec["file"], root / spec["rendererFile"]
        edge = "      , (ProfileSuspended, ProfilePublished), (ProfileSuspended, ProfileArchived)\n"
        assert policy.count(edge) == 1
        target.write_text(policy.replace(edge, "      , (ProfileSuspended, ProfileArchived)\n", 1))
        expect("edge removed from the relation", gate.check_code(bindings, machines, root),
               "code transitionsOnlyDeclared is [['suspended', 'published']]")
        target.write_text(policy.replace(edge, edge + "      , (ProfileArchived, ProfileDraft)\n", 1))
        expect("edge added to the relation", gate.check_code(bindings, machines, root),
               "code transitionsOnlyImplemented is [['archived', 'draft']]")
        target.write_text(policy.replace(edge, "      , (ProfileSuspended, ProfilePublished), (ProfileSuspended, ProfileRetired)\n", 1))
        expect("relation constructor without stored name", gate.check_code(bindings, machines, root),
               "ProfileRetired has no stored name")
        target.write_text(policy.replace(edge, "      , (ProfileSuspended, ProfilePublished), (ProfileSuspended, ProfileArchived)\n      ] <> reopenTransitions\n      where reopenTransitions = [\n", 1))
        expect("relation set extended by another expression", gate.check_code(bindings, machines, root),
               "allowedProfileTransition transitions is not a single literal set")
        target.write_text(policy.replace(edge, "      , (ProfileSuspended, ProfilePublished), reopen\n", 1))
        expect("named entry inside the relation set", gate.check_code(bindings, machines, root),
               "allowedProfileTransition transitions has an entry that is not a literal pair")
        target.write_text(policy.replace("  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions\n  where\n    transitions = Set.fromList\n      [ (ProfileDraft, ProfilePendingReview)",
                                         "  fromStatus == toStatus || True\n  where\n    transitions = Set.fromList\n      [ (ProfileDraft, ProfilePendingReview)", 1))
        expect("relation that ignores its set", gate.check_code(bindings, machines, root),
               "allowedProfileTransition does not decide membership in transitions")
        target.write_text(policy.replace(edge, "      -- , (ProfileSuspended, ProfilePublished)\n      , (ProfileSuspended, ProfileArchived)\n", 1))
        expect("relation edge kept only in a comment", gate.check_code(bindings, machines, root),
               "code transitionsOnlyDeclared is [['suspended', 'published']]")
        decision = "  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions\n  where\n    transitions = Set.fromList\n      [ (ProfileDraft, ProfilePendingReview)"
        assert policy.count(decision) == 1
        target.write_text(policy.replace(decision, decision.replace("`Set.member` transitions\n", "`Set.member` transitions || toStatus == ProfileArchived\n"), 1))
        expect("predicate with an extra disjunct", gate.check_code(bindings, machines, root),
               "allowedProfileTransition does not decide membership in transitions")
        target.write_text(policy.replace(decision, decision.replace("  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions\n  where\n", "  True\n  where\n    _member = (fromStatus, toStatus) `Set.member` transitions\n"), 1))
        expect("membership kept only in an unused helper", gate.check_code(bindings, machines, root),
               "allowedProfileTransition does not decide membership in transitions")
        target.write_text(policy.replace("allowedProfileTransition fromStatus toStatus =\n", "allowedProfileTransition ProfileArchived _ = True\nallowedProfileTransition fromStatus toStatus =\n", 1))
        expect("specific clause before the relation clause", gate.check_code(bindings, machines, root),
               "allowedProfileTransition must be a single clause")
        target.write_text(policy.replace(decision, decision.replace("    transitions = Set.fromList\n", "    transitions = Set.insert (ProfileArchived, ProfileDraft) base\n    base = Set.fromList\n"), 1))
        expect("transitions built from a decoy set", gate.check_code(bindings, machines, root),
               "allowedProfileTransition must bind transitions = Set.fromList once")
        original = "    transitions = Set.fromList\n"
        # The real binding adds an undeclared edge, while an unused helper's
        # nested binding contains the old literal table. Both layouts compiled
        # by Haskell must be rejected rather than certified from that decoy.
        for layout, replacement in (
            ("nested let", "    transitions = Set.insert (ProfileArchived, ProfileDraft) baseTransitions\n"
             "    baseTransitions = Set.empty\n"
             "    helper =\n        let\n          transitions = Set.fromList\n"),
            ("nested where", "    transitions = Set.insert (ProfileArchived, ProfileDraft) baseTransitions\n"
             "    baseTransitions = Set.empty\n"
             "    helper = transitions\n      where\n        transitions = Set.fromList\n"),
        ):
            start = policy.index(original, policy.index("allowedProfileTransition fromStatus"))
            end = policy.index("      ]", start) + len("      ]")
            table = policy[start:end]
            nested = table.replace(original, replacement, 1)
            # Keep the literal list inside the nested declaration's layout.
            nested = nested.replace("\n      [", "\n            [").replace("\n      ,", "\n            ,").replace("\n      ]", "\n            ]")
            if layout == "nested let":
                nested += "\n        in transitions"
            target.write_text(policy[:start] + nested + policy[end:])
            expect(f"shadowed transitions in {layout}", gate.check_code(bindings, machines, root),
                   "allowedProfileTransition must bind transitions = Set.fromList once")
        target.write_text(policy.replace(decision, decision.replace("`Set.member` transitions\n", "`Set.member` transitions'\n").replace("    transitions = Set.fromList\n", "    transitions' = Set.insert (ProfileArchived, ProfileDraft) transitions\n    transitions = Set.fromList\n"), 1))
        expect("predicate deciding on a primed set", gate.check_code(bindings, machines, root),
               "allowedProfileTransition does not decide membership in transitions")
        target.write_text(policy)
        renderer.write_text(server.replace("parseProfileStatus value = lookup value\n", 'parseProfileStatus "suspended" = Just ProfileMerged\nparseProfileStatus value = lookup value\n', 1))
        expect("specific clause before the parse table", gate.check_code(bindings, machines, root),
               "parseProfileStatus must be a single parse-table clause")
        target.write_text(policy)
        mapping = '("suspended",ProfileSuspended)'
        assert server.count(mapping) == 1
        renderer.write_text(server.replace(mapping, '("suspended",ProfileSuspended),("suspended",ProfileMerged)', 1))
        expect("parse table maps a stored name twice", gate.check_code(bindings, machines, root),
               "parseProfileStatus maps a stored name more than once")
        renderer.write_text(server.replace(mapping, 'suspendedEntry', 1))
        expect("parse table entry that is not a literal pair", gate.check_code(bindings, machines, root),
               "parseProfileStatus parse table has an entry that is not a literal pair")
        renderer.write_text(server.replace(mapping + ",", "", 1))
        expect("parse table entry removed", gate.check_code(bindings, machines, root), "ProfileSuspended has no stored name")
    unbound = copy.deepcopy(bindings)
    del unbound["machines"][CLAIM]["code"]
    expect("required relation binding removed", gate.check_code(unbound, machines),
           f"{CLAIM}: required backend validator binding is missing")
    return 21

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

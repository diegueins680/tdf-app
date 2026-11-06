#!/usr/bin/env python3
"""Tests for scripts/check-route-requirements.py (SYS-ROUTE-REQUIREMENTS-001)."""

import copy
import importlib.util
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parent / "check-route-requirements.py"
spec = importlib.util.spec_from_file_location("check_route_requirements", SCRIPT)
routes = importlib.util.module_from_spec(spec)
spec.loader.exec_module(routes)


def op(method, path):
    return {"method": method, "path": path}


SURFACE = {"operations": [
    op("GET", "/invoices"),
    op("POST", "/invoices/{sessionId}/generate"),
    op("POST", "/parties"),
    op("PUT", "/things/{id}"),
    op("DELETE", "/things/{id}"),
    op("POST", "/other"),
]}
REQUIREMENTS = {"requirements": [
    {"id": "PAY-1", "statement": "POST /invoices/{id}/generate issues once."},
    {"id": "CRM-1", "statement": "Contacts are created through /parties."},
    {"id": "THING-1", "statement": "Things are edited at /things/{thingId}."},
]}


class RouteRequirementsTest(unittest.TestCase):
    def ledger(self):
        return routes.build(SURFACE, REQUIREMENTS)

    def entry(self, ledger, method, path):
        return next(e for e in ledger["routes"] if (e["method"], e["path"]) == (method, path))

    def test_build_covers_only_mutating_routes_with_their_basis(self):
        ledger = self.ledger()
        self.assertEqual(ledger["summary"], {
            "total": 5, "named": 1, "path-named": 1, "reviewed": 0, "unspecified": 3, "covered": 2})
        # Parameters compare by position, not by name.
        self.assertEqual(self.entry(ledger, "POST", "/invoices/{sessionId}/generate")["requirements"], ["PAY-1"])
        self.assertEqual(self.entry(ledger, "POST", "/parties")["basis"], "path-named")
        # A path with two mutating methods is not attributed without a method.
        self.assertEqual(self.entry(ledger, "PUT", "/things/{id}")["basis"], "unspecified")
        self.assertEqual(routes.check(SURFACE, REQUIREMENTS, ledger), [])

    def test_new_mutating_route_must_be_classified(self):
        surface = copy.deepcopy(SURFACE)
        surface["operations"].append(op("PATCH", "/new"))
        errors = routes.check(surface, REQUIREMENTS, self.ledger())
        self.assertTrue(any("missing from the ledger: PATCH /new" in e for e in errors), errors)

    def test_removed_route_is_stale(self):
        surface = {"operations": SURFACE["operations"][:-1]}
        errors = routes.check(surface, REQUIREMENTS, self.ledger())
        self.assertTrue(any("stale ledger entry" in e and "/other" in e for e in errors), errors)

    def test_requirement_text_change_forces_regeneration(self):
        ledger = self.ledger()
        changed = copy.deepcopy(REQUIREMENTS)
        changed["requirements"][0]["statement"] = "Invoices issue once."
        errors = routes.check(SURFACE, changed, ledger)
        self.assertTrue(any("regenerate" in e and "/generate" in e for e in errors), errors)

    def test_claimed_coverage_must_match_the_text(self):
        ledger = self.ledger()
        entry = self.entry(ledger, "POST", "/other")
        entry.update({"basis": "named", "requirements": ["PAY-1"]})
        ledger["summary"] = routes.summarize(ledger["routes"])
        self.assertTrue(routes.check(SURFACE, REQUIREMENTS, ledger))

    def test_reviewed_entry_needs_known_requirements_and_a_note_and_survives_write(self):
        ledger = self.ledger()
        entry = self.entry(ledger, "POST", "/other")
        entry.update({"basis": "reviewed", "requirements": ["THING-1"]})
        ledger["summary"] = routes.summarize(ledger["routes"])
        self.assertTrue(any("needs requirements and a note" in e for e in routes.check(SURFACE, REQUIREMENTS, ledger)))
        entry["note"] = "Handler otherThing writes the same thing record."
        self.assertEqual(routes.check(SURFACE, REQUIREMENTS, ledger), [])
        rebuilt = routes.build(SURFACE, REQUIREMENTS, ledger)
        self.assertEqual(self.entry(rebuilt, "POST", "/other")["basis"], "reviewed")
        entry["requirements"] = ["NOPE-1"]
        self.assertTrue(any("unknown requirement" in e for e in routes.check(SURFACE, REQUIREMENTS, ledger)))

    def test_duplicate_entry_and_wrong_summary_fail(self):
        ledger = self.ledger()
        ledger["routes"].append(dict(ledger["routes"][0]))
        errors = routes.check(SURFACE, REQUIREMENTS, ledger)
        self.assertTrue(any("duplicate ledger entry" in e for e in errors))
        self.assertTrue(any("summary" in e for e in errors))

    def test_repository_ledger_is_current(self):
        self.assertEqual(routes.main([]), 0)


if __name__ == "__main__":
    unittest.main()

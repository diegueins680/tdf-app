#!/usr/bin/env python3
"""Regression checks for artifact coverage without promoting discovery to authority."""
import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location("inventory", Path(__file__).with_name("specification-inventory.py"))
inventory = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inventory)


class DiscoveryCoverage(unittest.TestCase):
    def test_component_contracts_are_discovered(self):
        for path in ["tdf-hq/docs/service_marketplace_formal_spec.md",
                     "tdf-hq/docs/CONTRACTS_API.md", "tdf-hq/docs/openapi/social-v2.yaml",
                     "tdf-hq-ui/README.md", "streaming/README.md", "tidal-agent/README.md",
                     "FORMAL_VERIFICATION.md", "SERVICE_STOREFRONT_DEPLOYMENT.md",
                     "formal/system/requirements.json", "formal/social/ConsentTraces.tla"]:
            with self.subTest(path=path):
                self.assertTrue(inventory.is_specification_candidate(path))

    def test_private_context_and_receipts_are_not_requirements(self):
        for path in ["MEMORY.md", "USER.md", "SOUL.md", "DREAMS.md", "TOOLS.md",
                     "memory/2026-09-20.md", "docs/campaigns/example.md",
                     "formal/system/evidence/README.md", "docs/reports/result.yaml",
                     "tdf-hq/docs/event-research-runs/pilot.json", "formal/system/inventory.json"]:
            with self.subTest(path=path):
                self.assertFalse(inventory.is_specification_candidate(path))

    def test_discovered_contract_is_not_automatically_approved(self):
        artifacts = {item["path"]: item for item in inventory.generate()["artifacts"]}
        self.assertEqual(artifacts["tdf-hq/docs/service_marketplace_formal_spec.md"]["authority"], "inferred")
        self.assertEqual(artifacts["tdf-hq/docs/openapi/social-v2.yaml"]["kind"], "interface-contract")


if __name__ == "__main__":
    unittest.main()

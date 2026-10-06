#!/usr/bin/env python3
"""Regression checks for artifact coverage without promoting discovery to authority."""
import importlib.util
from pathlib import Path
import unittest
import subprocess
import tempfile

spec = importlib.util.spec_from_file_location("inventory", Path(__file__).with_name("specification-inventory.py"))
inventory = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inventory)


class DiscoveryCoverage(unittest.TestCase):
    def test_mobile_must_be_initialized_and_at_the_pinned_revision(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory).resolve()
            mobile = root/'tdf-mobile'
            mobile.mkdir()
            def git(repo, *args):
                return subprocess.check_output(['git', '-c', 'user.name=Test', '-c',
                    'user.email=test@example.invalid', '-c', 'commit.gpgsign=false', *args],
                    cwd=repo, stderr=subprocess.DEVNULL, text=True).strip()
            for repo in [root, mobile]:
                git(repo, 'init', '-q')
                git(repo, 'commit', '--allow-empty', '-qm', 'fixture')
            pin = git(mobile, 'rev-parse', 'HEAD')
            git(root, 'update-index', '--add', '--cacheinfo', f'160000,{pin},tdf-mobile')
            git(root, 'commit', '-qm', 'pin')
            self.assertEqual(inventory.mobile_paths(root), (pin, []))
            git(mobile, 'commit', '--allow-empty', '-qm', 'different')
            with self.assertRaisesRegex(ValueError, 'differs'):
                inventory.mobile_paths(root)
            (mobile/'.git').rename(root/'saved-mobile-git')
            with self.assertRaisesRegex(ValueError, 'Initialize'):
                inventory.mobile_paths(root)

    def test_component_contracts_are_discovered(self):
        for path in ["tdf-hq/docs/service_marketplace_formal_spec.md",
                     "tdf-hq/docs/CONTRACTS_API.md", "tdf-hq/docs/openapi/social-v2.yaml",
                     "tdf-hq-ui/README.md", "streaming/README.md", "tidal-agent/README.md",
                     "FORMAL_VERIFICATION.md", "SERVICE_STOREFRONT_DEPLOYMENT.md",
                     "formal/system/requirements.json", "formal/social/ConsentTraces.tla",
                     "ops/hetzner/README.md", "ops/hetzner/compose.production.yaml",
                     "tdf-hq-ui/public/data-deletion.html", "tdf-hq-ui/public/privacy.html",
                     "tdf-hq-ui/public/account/privacy-es.html", "tdf-hq-ui/public/mobile-app/terms.html",
                     "tdf-mobile/README.md", "tdf-mobile/docs/social-contract.md"]:
            with self.subTest(path=path):
                self.assertTrue(inventory.is_specification_candidate(path))

    def test_private_context_and_receipts_are_not_requirements(self):
        for path in ["MEMORY.md", "USER.md", "SOUL.md", "DREAMS.md", "TOOLS.md",
                     "memory/2026-09-20.md", "docs/campaigns/example.md",
                     "formal/system/evidence/README.md", "docs/reports/result.yaml", "tdf-mobile/AGENTS.md",
                     "tdf-hq/docs/event-research-runs/pilot.json", "formal/system/inventory.json"]:
            with self.subTest(path=path):
                self.assertFalse(inventory.is_specification_candidate(path))

    def test_discovered_contract_is_not_automatically_approved(self):
        artifacts = {item["path"]: item for item in inventory.generate()["artifacts"]}
        self.assertEqual(artifacts["tdf-hq/docs/service_marketplace_formal_spec.md"]["authority"], "inferred")
        self.assertEqual(artifacts["tdf-hq/docs/openapi/social-v2.yaml"]["kind"], "interface-contract")
        self.assertEqual(artifacts["specs.yaml"]["authority"], "historical")
        self.assertEqual(artifacts["formal/system/history-2026-09-20.md"]["authority"], "historical")
        self.assertEqual(artifacts["tdf-hq-ui/public/data-deletion.html"]["authority"], "inferred")
        self.assertEqual(artifacts["tdf-hq-ui/public/data-deletion.html"]["kind"], "published-product-page")

    def test_material_boundaries_are_fingerprinted(self):
        result = inventory.generate()
        surfaces = {item['path']: item for item in result['surfaces']}
        for path in ['tdf-mobile/src/lib/queryClient.ts', 'tdf-mobile/src/api/generated/types.ts',
                     'ops/hetzner/compose.production.yaml', 'tdf-hq/src/TDF/Auth.hs',
                     'tdf-hq/assets/feature-registry.json', 'scripts/production-migrations.json']:
            with self.subTest(path=path):
                self.assertEqual(surfaces[path]['sha256'], inventory.digest((inventory.ROOT/path).read_bytes()))
                self.assertEqual(surfaces[path]['semanticConformance'], 'not-established-by-discovery')
        self.assertRegex(result['mobileRevision'], r'^[0-9a-f]{40}$')

    def test_every_shipped_html_page_is_indexed_and_fingerprinted(self):
        result = inventory.generate()
        artifacts = {item['path']: item for item in result['artifacts']}
        surfaces = {item['path']: item for item in result['surfaces']}
        pages = list((inventory.ROOT/'tdf-hq-ui/public').rglob('*.html'))
        self.assertGreaterEqual(len(pages), 13)
        for page in pages:
            path = str(page.relative_to(inventory.ROOT))
            with self.subTest(path=path):
                self.assertEqual(artifacts[path]['kind'], 'published-product-page')
                self.assertEqual(surfaces[path]['sha256'], inventory.digest(page.read_bytes()))
                self.assertEqual(artifacts[path]['authority'], 'inferred')


if __name__ == "__main__":
    unittest.main()

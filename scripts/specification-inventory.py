#!/usr/bin/env python3
"""Deterministic discovery index. Discovery is not authority or verification."""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent
OUTPUT = ROOT / "formal/system/inventory.json"


def digest(data):
    return hashlib.sha256(data).hexdigest()


def generate():
    paths = sorted(set(subprocess.check_output(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"], cwd=ROOT
    ).decode().strip("\0").split("\0")))
    artifacts, models, requirements, components = [], [], [], {}
    for relative in paths:
        path = ROOT / relative
        if not path.is_file() or path.is_symlink() or any(p in path.parts for p in ("node_modules", "__pycache__")):
            continue
        if relative.startswith(("tdf-hq/", "tdf-hq-ui/", "scripts/", "functions/", "streaming/", "tidal-agent/", ".github/")):
            component = relative.split('/')[0]
            components.setdefault(component, []).append(relative)
        # Source specifications, excluding historical evidence, private working notes and campaign material.
        candidate = (relative in ("specs.yaml", "FORMAL_VERIFICATION.md", "README.md", "MOBILE_APP.md", "FEATURES.md",
                                  "PAYMENT_AUDIT.md", "VERIFICATION.md", "release-readiness.md", "DEPLOYMENT_GUIDE.md")
                     or (relative.startswith("docs/") and path.suffix in (".md", ".yaml")
                         and not any(x in relative.split('/') for x in ("evidence", "campaigns", "reports", "rollout-evidence")))
                     or relative == "tdf-hq/docs/openapi/api.yaml"
                     or relative.startswith("formal/") and path.suffix in (".tla", ".cfg", ".als"))
        if not candidate:
            continue
        data = path.read_bytes()
        source = data.decode()
        declaration = re.search(r"(?im)^(?:Status|Estado):\s*(.+)$", source)
        status = "inferred"
        if relative.startswith("docs/adr/") and declaration:
            value = declaration[1].lower()
            if value.startswith(("accepted", "aceptado", "aceptada")):
                status = "approved"
            elif value.startswith("proposed"):
                status = "proposed"
        artifacts.append({"path": relative, "sha256": digest(data), "authority": status,
                          "authorityBasis": declaration[0] if declaration else "No explicit approval declaration extracted; retain source provenance for review",
                          "kind": "formal-model" if path.suffix in (".tla", ".als") else
                          "model-configuration" if path.suffix == ".cfg" else
                          "interface-contract" if relative.endswith("openapi/api.yaml") else "requirement-candidate"})
        if path.suffix == ".cfg":
            models.append({"configuration": relative, "declarations": source.splitlines(),
                           "result": "not-revalidated-by-inventory", "correspondence": "See domain traceability; discovery establishes no refinement"})
        if relative.startswith("docs/adr/"):
            match = re.search(r"(?ms)^## Decisi[oó]n\s*\n(.*?)(?=^## |\Z)", source)
            if match:
                for statement in re.split(r"\n\s*\n", match[1].strip()):
                    statement = " ".join(statement.split())
                    if not statement:
                        continue
                    requirements.append({"id": f"{path.stem}:{digest(statement.encode())[:12]}",
                                         "statement": statement, "source": relative + ("#decisión" if "## Decisión" in source else "#decision"), "status": status,
                                         "deliveryScope": "Current-baseline applicability requires the domain traceability/rollout decision; approval is not automatic activation",
                                         "verification": "open", "gap": "Per-clause implementation correspondence not discharged by inventory"})
        if relative == "specs.yaml":
            ancestry = []
            for number, line in enumerate(source.splitlines(), 1):
                if not line.strip() or line.lstrip().startswith('#'):
                    continue
                indentation = len(line) - len(line.lstrip())
                while ancestry and ancestry[-1][0] >= indentation:
                    ancestry.pop()
                key = "/".join(value for _, value in ancestry) + "/" + line.strip()
                ancestry.append((indentation, line.strip()))
                requirements.append({"id": "LEGACY-" + digest(key.encode())[:12],
                                     "statement": line.strip(), "source": f"specs.yaml:{number}",
                                     "status": "inferred", "deliveryScope": "Legacy v1 candidate; approval/supersession unresolved",
                                     "verification": "open", "gap": "YAML line is discovery evidence; contextual interpretation and approval remain required"})
        if relative == "docs/payments/ecuador-payment-platform-audit-2026-09-09.md":
            # Approval is local to this embedded ADR, not the surrounding research report.
            section = re.search(r"(?ms)^### ADR-0200 .*?(?=^## 6\.)", source)
            if not section or "**Status:** accepted for implementation." not in section[0]:
                raise SystemExit("ADR-0200 authority changed; review extraction and applicability")
            invariants = re.search(r"(?ms)^### Invariants\n(.*?)(?=^### )", section[0])
            if not invariants:
                raise SystemExit("ADR-0200 invariants missing; review extraction")
            for number, statement in re.findall(r"(?m)^(\d+)\. (.+)$", invariants[1]):
                requirements.append({"id": "ADR-0200-INV-" + number.zfill(2),
                                     "statement": statement, "source": relative + "#invariants",
                                     "status": "approved", "authorityBasis": "Embedded ADR-0200: accepted for implementation",
                                     "deliveryScope": "Canonical payment core; provider activation and future business flows retain their separate delivery gates",
                                     "verification": "open", "gap": "Arithmetic slice traced in requirements.json; remaining clauses require implementation correspondence"})
        if relative == "docs/event-operations/gap-matrix.md":
            for line in source.splitlines():
                cells = [x.strip() for x in line.split('|')]
                if len(cells) >= 5 and re.fullmatch(r"EO-\d+", cells[1]):
                    requirements.append({"id": cells[1], "statement": cells[2], "source": relative,
                                         "status": "proposed", "deliveryScope": "Historical event roadmap; do not classify absence as current defect without applicability",
                                         "verification": "open", "gap": "See existing domain traceability matrix and current code; historical classification is not current execution evidence"})
    return {"schemaVersion": 1, "purpose": "Comprehensive discovery of indexed artifact classes; not a claim of complete semantic requirements extraction",
            "components": components, "artifacts": artifacts, "requirements": requirements, "modelConfigurations": models,
            "exclusions": ["Mobile source is a separate Git repository; exact gitlink is recorded in baseline and execution manifests",
                           "Binary/media files, personal notes, historical evidence and campaigns are not treated as requirements",
                           "Implicit rules in every handler/schema are not exhaustively extracted; open system-level obligation SYS-GAP-01"]}


if __name__ == "__main__":
    if sys.argv[1:] not in ([], ["--check"]):
        raise SystemExit("Usage: specification-inventory.py [--check]")
    rendered = json.dumps(generate(), ensure_ascii=False, indent=2) + "\n"
    register = json.loads((ROOT / "formal/system/requirements.json").read_text())
    ids = set()
    for requirement in register["requirements"]:
        for key in ("id", "statement", "authority", "source", "property", "contract", "implementation", "verification", "evidence", "status"):
            if not requirement.get(key):
                raise SystemExit(f"Missing traceability field {key}: {requirement.get('id')}")
        if requirement["id"] in ids:
            raise SystemExit("Duplicate requirement ID: " + requirement["id"])
        ids.add(requirement["id"])
        for reference in [requirement["contract"], *requirement["implementation"]]:
            if not (ROOT / reference).is_file():
                raise SystemExit("Missing traceability target: " + reference)
    if sys.argv[1:] == ["--check"]:
        if not OUTPUT.exists() or OUTPUT.read_text() != rendered:
            raise SystemExit("Specification discovery index is stale; regenerate and review its changes")
    else:
        OUTPUT.write_text(rendered)
    print("Specification discovery index " + ("checked" if sys.argv[1:] else "generated"))

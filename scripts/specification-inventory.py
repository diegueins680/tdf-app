#!/usr/bin/env python3
"""Deterministic discovery index. Discovery is not authority or verification."""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys
from lib.requirement_declarations import extract_declarations

ROOT = Path(__file__).resolve().parent.parent
OUTPUT = ROOT / "formal/system/inventory.json"


def digest(data):
    return hashlib.sha256(data).hexdigest()


def is_specification_candidate(relative):
    path = Path(relative)
    # Root technical guidance is evidence, not automatically approved policy.
    private_roots = {"AGENTS.md", "SOUL.md", "USER.md", "MEMORY.md", "DREAMS.md",
                     "HEARTBEAT.md", "IDENTITY.md", "TOOLS.md", "memory-storage-analysis.md"}
    if len(path.parts) == 1:
        return relative == "specs.yaml" or path.suffix == ".md" and relative not in private_roots
    if any(part in {"evidence", "campaigns", "reports", "rollout-evidence", "event-research-runs"}
           for part in path.parts):
        return False
    if relative.startswith(("docs/", "tdf-hq/docs/", "tdf-mobile/docs/", "ops/")):
        return path.suffix in (".md", ".yaml")
    if path.parts[0] == "tdf-mobile" and len(path.parts) == 2:
        return path.suffix == ".md" and path.name not in private_roots
    if relative.startswith("formal/"):
        return path.suffix in (".md", ".tla", ".cfg", ".als") or relative in {
            "formal/system/requirements.json", "formal/system/authority-decisions.json",
            "formal/system/research.json", "formal/system/dependency-security.json"}
    return path.name == "README.md" and path.parts[0] in {
        "tdf-hq", "tdf-hq-ui", "scripts", "functions", "streaming", "tidal-agent"}


def mobile_paths(root):
    """Require pinned source; an empty submodule cannot establish coverage."""
    mobile = root / "tdf-mobile"
    if mobile.is_symlink() or not mobile.resolve().is_relative_to(root.resolve()):
        raise ValueError("Mobile source must be contained in the audit checkout")
    entry = subprocess.check_output(
        ["git", "ls-tree", "HEAD", "--", "tdf-mobile"], cwd=root, text=True).strip()
    match = re.fullmatch(r"160000 commit ([0-9a-f]{40})\ttdf-mobile", entry)
    if not match:
        raise ValueError("Missing committed Mobile gitlink")
    top = subprocess.check_output(["git", "rev-parse", "--show-toplevel"], cwd=mobile, text=True).strip()
    if Path(top).resolve() != mobile.resolve():
        raise ValueError("Initialize exact pinned Mobile before specification discovery")
    revision = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=mobile, text=True).strip()
    if revision != match[1]:
        raise ValueError("Mobile checkout differs from the committed gitlink")
    paths = subprocess.check_output(["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
                                    cwd=mobile).decode().split("\0")
    return revision, ["tdf-mobile/" + p for p in paths if p]


def material_source(relative):
    path = Path(relative)
    roots = {"tdf-hq", "tdf-hq-ui", "tdf-mobile", "scripts", "functions", "streaming",
             "tidal-agent", "ops", "e2e", "test", ".github"}
    suffixes = {".hs", ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".py", ".sh",
                ".sql", ".json", ".yaml", ".yml", ".toml", ".cabal"}
    return (path.parts[0] in roots and (path.suffix in suffixes or path.name.startswith("Dockerfile"))) \
        or relative in {"package.json", "package-lock.json", "fly.toml", ".gitmodules"}


def generate():
    paths = sorted(set(subprocess.check_output(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"], cwd=ROOT
    ).decode().strip("\0").split("\0")))
    mobile_revision, mobile_files = mobile_paths(ROOT)
    paths = sorted(set(paths + mobile_files))
    artifacts, models, requirements, components, surfaces = [], [], [], {}, []
    for relative in paths:
        path = ROOT / relative
        relative_path = Path(relative)
        if (not path.is_file() or not path.resolve().is_relative_to(ROOT.resolve())
                or any((ROOT/p).is_symlink() for p in (relative_path, *relative_path.parents))
                or any(p in path.parts for p in ("node_modules", "__pycache__"))):
            continue
        if material_source(relative):
            component = relative.split('/')[0]
            components.setdefault(component, []).append(relative)
            surfaces.append({"id": "SURFACE-" + digest(relative.encode())[:16], "path": relative,
                             "sha256": digest(path.read_bytes()),
                             "semanticConformance": "not-established-by-discovery"})
        # Source specifications, excluding historical evidence, private working notes and campaign material.
        if not is_specification_candidate(relative):
            continue
        data = path.read_bytes()
        source = data.decode()
        declaration = re.search(r"(?im)^(?:Status|Estado):\s*(.+)$", source)
        status = "inferred"
        if relative.startswith("docs/archive/") or relative == "specs.yaml" or (declaration and declaration[1].lower().startswith("historical")):
            status = "historical"
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
                          "interface-contract" if "/openapi/" in relative and path.suffix == ".yaml" else
                          "requirement-register" if relative == "formal/system/requirements.json" else "requirement-candidate"})
        if path.suffix == ".cfg":
            models.append({"configuration": relative, "declarations": source.splitlines(),
                           "result": "not-revalidated-by-inventory", "correspondence": "See domain traceability; discovery establishes no refinement"})
        if path.suffix == '.md' and relative != 'docs/event-operations/gap-matrix.md':
            # Preserve authored contract identifiers independently of their line
            # number/text. This is discovery, not approval or current evidence.
            requirements.extend(extract_declarations(source, relative, status))
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
                                     "status": "historical", "deliveryScope": "Legacy v1 candidate; superseded as current system authority by formal/system/README.md; per-clause applicability requires reconciliation",
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
    return {"schemaVersion": 2, "purpose": "Discovery of indexed artifact classes with exact pinned Mobile; not a claim of complete semantic requirements extraction",
            "mobileRevision": mobile_revision,
            "components": components, "surfaces": surfaces, "artifacts": artifacts, "requirements": requirements, "modelConfigurations": models,
            "exclusions": ["Binary/media files and native generated build outputs are not semantically verified",
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

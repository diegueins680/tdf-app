#!/usr/bin/env python3
"""AUTH-ADMIN-GUARD-001: every leaf handler of the /admin API enforces an
authorization guard.

The handlers live in the `where` block of `adminServer` in
tdf-hq/src/TDF/ServerAdmin.hs (and of `recordsIngestionServer` in
tdf-hq/src/TDF/Server/RecordsIngestion.hs). A handler is a leaf when it does
not merely delegate to other handlers. This check reads the source: it is a
structural gate against a forgotten guard, not a proof of what each guard
decides (AUTH-REGISTRY-001 covers the predicates).

Exit 0 when every leaf handler contains a guard call; exit 1 with the list of
unguarded handlers otherwise. Pass --json for machine-readable output.
"""
import json
import os
import re
import sys
from pathlib import Path

ROOT = Path(os.environ.get('TDF_ADMIN_GUARD_ROOT') or Path(__file__).resolve().parents[1])

GUARD = re.compile(r'\b(ensureStrictAdmin|ensureModule|ensureAdminSeedToken|requireAdmin'
                   r'|requireStrictAdmin|requireModule|validateModuleAccess|hasStrictAdminAccess)\b')
DEF = re.compile(r'^    ([a-z][A-Za-z0-9_\']*)\b.*=\s*(do)?\s*$')
SKIP = {'let', 'where', 'in'}

# Helpers in the same where-block that are not route handlers. Each is called
# from a guarded handler; listing one here is a reviewed decision.
HELPERS = {
    'tdf-hq/src/TDF/ServerAdmin.hs': {
        'recordActivity', 'unholdByExternalId', 'unholdLatestBySender',
        'artistPromoLocalePreferences', 'resolveArtistKey', 'missingModuleBody',
        'channel', 'missingChannel', 'invalid', 'storedPhone', 'authority', 'step',
    },
    'tdf-hq/src/TDF/Server/RecordsIngestion.hs': {'db'},
}


def handlers(path):
    lines = (ROOT / path).read_text().split('\n')
    defs = []
    for index, line in enumerate(lines):
        match = DEF.match(line)
        if match and match.group(1) not in SKIP:
            defs.append((index, match.group(1)))
    result = []
    for position, (start, name) in enumerate(defs):
        end = defs[position + 1][0] if position + 1 < len(defs) else len(lines)
        body = '\n'.join(lines[start:end])
        head = '\n'.join(lines[start:start + 3])
        names = {other for _, other in defs}
        # A router's head names other handlers of the same block (any identifier
        # defined there), so suffix conventions are not relied upon.
        delegates = {d for d in re.findall(r'\b([a-z][A-Za-z0-9_\']*)\b', head)
                     if d != name and d in names}
        guarded = bool(GUARD.search(body))
        result.append({'file': path, 'line': start + 1, 'name': name,
                       'guarded': guarded, 'delegates': sorted(delegates)})
    return result


def main():
    want_json = '--json' in sys.argv[1:]
    report = []
    unguarded = []
    for path in ['tdf-hq/src/TDF/ServerAdmin.hs', 'tdf-hq/src/TDF/Server/RecordsIngestion.hs']:
        for row in handlers(path):
            helper = row['name'] in HELPERS.get(path, set())
            leaf = not row['delegates'] and not helper
            row['leaf'] = leaf
            report.append(row)
            if leaf and not row['guarded']:
                unguarded.append(row)
    if want_json:
        print(json.dumps({'ok': not unguarded, 'handlers': report, 'unguarded': unguarded}, indent=2))
    else:
        leaves = [r for r in report if r['leaf']]
        print(f'Admin guard sweep: {len(leaves)} leaf handlers, {len(unguarded)} unguarded')
        for row in unguarded:
            print(f"  {row['file']}:{row['line']} {row['name']}")
    return 1 if unguarded else 0


if __name__ == '__main__':
    sys.exit(main())

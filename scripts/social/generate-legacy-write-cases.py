#!/usr/bin/env python3
"""Export observed Commit transitions, without recomputing authorization/effects."""
import json
import re
import sys
from pathlib import Path

nodes, edges = {}, []
for line in Path(sys.argv[1]).read_text().splitlines():
    edge = re.match(r'(-?\d+) -> (-?\d+) \[label="([^"]+)"', line)
    if edge:
        edges.append(edge.groups())
        continue
    node = re.match(r'(-?\d+) \[label=("(?:\\.|[^"\\])*")', line)
    if node:
        nodes[node[1]] = dict(re.findall(r'/\\ (\w+) = (.*?)(?=\n/\\ |$)', json.loads(node[2]), re.S))
cases = set()
for start, end, _ in edges:
    before, after = nodes[start], nodes[end]
    if before['phase'] != '"checked"' or after['phase'] != '"done"':
        continue
    success = re.search(r'success \|-> (TRUE|FALSE)', after['observed'])
    assert success
    booleans = tuple(before[k] == 'TRUE' for k in ('enabled', 'activated', 'pair', 'closed', 'token'))
    history = lambda s: tuple(map(int, re.findall(r'\d+', s['history'])))
    cases.add(booleans + (before['operation'].strip('"'), history(before), success[1] == 'TRUE', history(after)))
assert cases
def hs(value):
    if isinstance(value, bool): return str(value)
    if isinstance(value, tuple): return '[' + ','.join(map(str, value)) + ']'
    return json.dumps(value)
lines = ['-- Generated from checked LegacyWrites Commit observations. Do not hand-edit.',
         'module LegacyWriteModelCases (legacyWriteCases) where',
         'legacyWriteCases :: [(Bool,Bool,Bool,Bool,Bool,String,[Int],Bool,[Int])]',
         'legacyWriteCases =']
for index, case in enumerate(sorted(cases)):
    lines.append(('  [ ' if index == 0 else '  , ') + '(' + ','.join(map(hs, case)) + ')')
lines.append('  ]')
Path(sys.argv[2]).write_text('\n'.join(lines) + '\n')
print(f'Generated {len(cases)} observations from {len(nodes)} checked states')

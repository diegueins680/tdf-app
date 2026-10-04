#!/usr/bin/env python3
"""Export observed Commit results from TLC; do not recompute policy or effects."""
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
    booleans = lambda s, keys: tuple(s[k] == 'TRUE' for k in keys.split())
    history = lambda s: tuple(sorted(map(int, re.findall(r'\d+', s['edges']))))
    cases.add(booleans(before, 'enabled activated governed eligible token subscription club profile') +
              (history(before), int(before['alerts']), success[1] == 'TRUE') +
              booleans(after, 'subscription profile') + (history(after), int(after['alerts'])))
assert cases
def hs(value):
    if isinstance(value, bool): return str(value)
    if isinstance(value, tuple): return '[' + ','.join(map(str, value)) + ']'
    return str(value)
lines = ['-- Generated from checked FanEffects Commit observations. Do not hand-edit.',
         'module FanEffectsModelCases (FanCase(..), fanCases) where',
         '-- runtime, governance, eligibility/session, source state, observed result/state',
         'data FanCase = FanCase Bool Bool Bool Bool Bool Bool Bool Bool [Int] Int Bool Bool Bool [Int] Int',
         '  deriving Show', 'fanCases :: [FanCase]', 'fanCases =']
for index, case in enumerate(sorted(cases)):
    lines.append(('  [ ' if index == 0 else '  , ') + 'FanCase ' + ' '.join(map(hs, case)))
lines.append('  ]')
Path(sys.argv[2]).write_text('\n'.join(lines) + '\n')
print(f'Generated {len(cases)} observations from {len(nodes)} checked states')

#!/usr/bin/env python3
"""Read outcomes from actual checked TLC Read transitions (one-request adapter)."""
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
        label = json.loads(node[2])
        nodes[node[1]] = dict(re.findall(r'/\\ (\w+) = (.*?)(?=\n/\\ |$)', label, re.S))
assert nodes and edges, 'Missing checked model graph'
cases = set()
for start, end, action in edges:
    if action != 'Read':
        continue
    before, after = nodes[start], nodes[end]
    def field(key):
        match = re.fullmatch(r'\(r1 :> (\w+)\)', before[key])
        assert match, f'Unexpected model bounds or {key} representation'
        return match[1]
    assert field('principal') == 'a', 'Adapter expects tokens owned by principal a'
    token = {'t1': 1, 't2': 6}[field('token')]
    actor = {'a': 1, 'b': 2}[field('acting')]
    allowed = re.search(r'allowed \|-> (TRUE|FALSE)', after['observed'])
    assert allowed, 'Missing independently observed result'
    cases.add((
        bool(re.search(r'\bt1\b', before['active'])),
        bool(re.search(r'\bt2\b', before['active'])),
        bool(re.search(r'\ba\b', before['alive'])),
        bool(re.search(r'\bb\b', before['alive'])),
        token, actor, allowed[1] == 'TRUE',
    ))
assert cases
lines = ['-- Generated from checked SessionBoundary Read transitions; do not hand-edit.',
         'module SessionModelCases (SessionCase(..), sessionCases) where',
         'import Data.Int (Int64)',
         '-- Active token 1/6, live actor 1/2, token id, acting id, observed grant.',
         'data SessionCase = SessionCase Bool Bool Bool Bool Int64 Int64 Bool deriving Show',
         'sessionCases :: [SessionCase]', 'sessionCases =']
for index, case in enumerate(sorted(cases)):
    values = ' '.join(str(v) for v in case)
    lines.append(('  [ ' if index == 0 else '  , ') + 'SessionCase ' + values)
lines.append('  ]')
Path(sys.argv[2]).write_text('\n'.join(lines)+'\n')
print(f'Generated {len(cases)} observed read cases from {len(nodes)} checked states')

#!/usr/bin/env python3
"""Generate actual INSERT expectations from checked LegacyDm observation edges."""
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
assert nodes and edges, 'Missing checked graph'
cases = set()
for start, end, action in edges:
    if action != 'Insert':
        continue
    before, after = nodes[start], nodes[end]
    def boolean(key):
        assert before[key] in ('TRUE','FALSE')
        return before[key] == 'TRUE'
    observed = re.search(r'allowed \|-> (TRUE|FALSE)', after['observed'])
    assert observed, 'Missing model observation'
    consent = set(re.findall(r'\b[12]\b',before['consent']))
    cases.add(tuple(boolean(k) for k in ('enabled','activated','exists','blocked','closed')) +
              ('1' in consent,'2' in consent,observed[1]=='TRUE'))
assert cases
out = ['-- Generated from checked LegacyDm Insert observations; do not hand-edit.']
for n, case in enumerate(sorted(cases),1):
    enabled, activated, exists, blocked, closed, a, b, allowed = map(lambda x:'true' if x else 'false',case)
    out += [f'-- Checked case {n}', 'BEGIN;',
      f'UPDATE social_v2_runtime SET enabled={enabled},activated_once={activated};',
      f'INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,{a},{b},{blocked} WHERE {exists};',
      f'INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE {closed};',
      'DO $$ DECLARE permitted boolean := true; BEGIN',
      "  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');",
      '  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;',
      f"  ASSERT permitted={allowed}, 'LegacyDm observed case {n} mismatch';",
      'END $$;', 'ROLLBACK;']
Path(sys.argv[2]).write_text('\n'.join(out)+'\n')
print(f'Generated {len(cases)} INSERT outcomes from {len(nodes)} checked model states')

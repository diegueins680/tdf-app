#!/usr/bin/env python3
"""Generate SQL transition assertions from TLC's actual action-labelled graph.

Only Request/Withdraw/Block/Unblock refine this adapter. Reject unknown actions;
this is deliberately not a claim about all Relationships.tla actions.
"""
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
        values = dict(re.findall(r'/\\ (\w+) = ([^\n]*)', label))
        nodes[node[1]] = values
assert nodes and edges, 'Missing checked TLC states or transitions'

def members(state, key):
    return set(re.findall(r'\b[ab]\b', state[key]))

def literal(value):
    return 'true' if value else 'false'

lines = ['-- Generated from TLC ConsentTraces graph; do not hand-edit.', 'DO $$ BEGIN']
cases = set()
for source_id, target_id, action in edges:
    before, after = nodes[source_id], nodes[target_id]
    if before == after:
        continue
    if action == 'Request':
        changed = members(after, 'consent') - members(before, 'consent')
        op = 'request'
    elif action == 'Block':
        changed = members(after, 'blocked') - members(before, 'blocked')
        op = 'block'
    elif action == 'Unblock':
        changed = members(before, 'blocked') - members(after, 'blocked')
        op = 'unblock'
    elif action == 'Withdraw':
        changed = members(before, 'consent') - members(after, 'consent')
        op = 'disconnect'
    else:
        raise AssertionError(f'Unmapped action {action}')
    assert len(changed) == 1
    actor, target = (3, 4) if changed == {'a'} else (4, 3)
    state_values = tuple(literal(actor in members(before, key))
                         for key in ('consent', 'blocked') for actor in ('a', 'b'))
    result_values = tuple(literal(actor in members(after, key))
                          for key in ('consent', 'blocked') for actor in ('a', 'b'))
    case = (state_values, before['version'], actor, op, result_values, after['version'])
    if case in cases:
        continue
    cases.add(case)
    n = len(cases)
    lines.extend([
        'DELETE FROM social_v2_command WHERE actor IN (3,4);',
        'DELETE FROM social_v2_pair WHERE party_a=3 AND party_b=4;',
        'INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a,block_b,revision) '
        f"VALUES(3,4,{','.join(state_values)},{before['version']});",
        f"ASSERT NOT (social_v2_mutate({actor},{target},'{op}',{before['version']},'model-{n}') ? 'error'), 'model action {n}';",
        f"ASSERT (SELECT ROW(consent_a,consent_b,block_a,block_b,revision)=ROW({','.join(result_values)},{after['version']}::bigint) "
        f"FROM social_v2_pair WHERE party_a=3 AND party_b=4), 'model state {n}';",
    ])
lines.extend(['END $$;', f"SELECT 'PASS: {len(cases)} TLC-generated consent transitions';"])
Path(sys.argv[2]).write_text('\n'.join(lines)+'\n')
print(f'Generated {len(cases)} transition assertions from {len(nodes)} checked states')

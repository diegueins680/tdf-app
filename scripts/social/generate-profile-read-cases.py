#!/usr/bin/env python3
"""Translate actual checked Read observations; no implementation policy duplicated here."""
import json,re,sys
from pathlib import Path
nodes,edges={},[]
for line in Path(sys.argv[1]).read_text().splitlines():
    edge=re.match(r'(-?\d+) -> (-?\d+) \[label="([^"]+)"',line)
    if edge: edges.append(edge.groups()); continue
    node=re.match(r'(-?\d+) \[label=("(?:\\.|[^"\\])*")',line)
    if node:
        nodes[node[1]]=dict(re.findall(r'/\\ (\w+) = (.*?)(?=\n/\\ |$)',json.loads(node[2]),re.S))
assert nodes and edges
cases=set()
def ids(value): return tuple({'a':1,'b':2,'z':999}[v] for v in re.findall(r'\b[abz]\b',value))
for start,end,action in edges:
    state,after=nodes[start],nodes[end]
    # Next's terminal guard combines TLC labels; only Read changes done.
    if state['done']!='FALSE' or after['done']!='TRUE': continue
    def boolean(key):
        assert state[key] in ('TRUE','FALSE')
        return state[key]=='TRUE'
    def has(key,actor): return bool(re.search(r'\b'+actor+r'\b',state[key]))
    returned=re.search(r'returned \|-> (<<.*?>>)',after['observed'],re.S); assert returned
    cases.add(tuple(boolean(k) for k in ('enabled','activated','pairExists','blocked'))+
        tuple(has(k,a) for k in ('closed','live') for a in ('a','b'))+
        tuple(boolean(k) for k in ('discoverable','muted'))+
        (ids(state['requested']),ids(returned[1])))
assert cases, 'No Read observations found'
lines=['-- Generated from checked ProfileReads observations; do not hand-edit.','DO $$ BEGIN']
def sql(v):
    if isinstance(v,bool): return 'true' if v else 'false'
    if isinstance(v,tuple): return 'ARRAY['+','.join(map(str,v))+']::bigint[]'
    return str(v)
for n,case in enumerate(sorted(cases),1):
    lines.append('  PERFORM assert_profile_read_case('+str(n)+','+','.join(map(sql,case))+');')
lines.append('END $$;')
Path(sys.argv[2]).write_text('\n'.join(lines)+'\n')
print(f'Generated {len(cases)} observed cases from {len(nodes)} checked states')

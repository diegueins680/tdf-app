#!/usr/bin/env python3
"""Read observed terminal transitions, without recomputing policy or counts."""
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
def pairs(value):
    return tuple(sorted(tuple({'a':1,'b':2,'c':3}[a] for a in pair)
        for pair in re.findall(r'<<([abc]),\s*([abc])>>',value)))
for start,end,_ in edges:
    state,after=nodes[start],nodes[end]
    if state['done']!='FALSE' or after['done']!='TRUE': continue
    def boolean(key):
        assert state[key] in ('TRUE','FALSE')
        return state[key]=='TRUE'
    def has(key,actor): return bool(re.search(r'\b'+actor+r'\b',state[key]))
    rows=re.search(r'rows \|-> (\{.*?\})',after['observed'],re.S); assert rows
    count=re.search(r'\bcount \|-> (\d+)',after['observed']); assert count
    cases.add(tuple(boolean(k) for k in ('enabled','activated','pairExists','blocked'))+
        tuple(has(k,a) for k in ('closed','live') for a in ('a','b'))+
        (state['route'].strip('"'),pairs(state['edges']),pairs(rows[1]),int(count[1])))
assert cases, 'No Read observations'
lines=['-- Generated from checked RelationshipReads observations; do not hand-edit.','DO $$ BEGIN']
def sql(v):
    if isinstance(v,bool): return 'true' if v else 'false'
    if isinstance(v,tuple): return "'"+json.dumps(v,separators=(',',':'))+"'::jsonb"
    if isinstance(v,str): return "'"+v+"'"
    return str(v)
for n,case in enumerate(sorted(cases),1):
    lines.append('  PERFORM assert_relationship_read_case('+str(n)+','+','.join(map(sql,case))+');')
lines.append('END $$;')
Path(sys.argv[2]).write_text('\n'.join(lines)+'\n')
print(f'Generated {len(cases)} observations from {len(nodes)} checked states')

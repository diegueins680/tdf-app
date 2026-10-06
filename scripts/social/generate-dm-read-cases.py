#!/usr/bin/env python3
"""Translate observed outcomes of checked DmReads Read transitions, not SQL policy."""
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
for start,end,action in edges:
    if action!='Read': continue
    state,after=nodes[start],nodes[end]
    def boolean(key):
        assert state[key] in ('TRUE','FALSE')
        return state[key]=='TRUE'
    def has(key,actor): return bool(re.search(r'\b'+actor+r'\b',state[key]))
    observed=after['observed']
    fields=re.search(r'fields \|-> (\{[^}]*\})',observed); assert fields
    allowed=fields[1]!='{}'
    participant=re.search(r'participant \|-> (TRUE|FALSE)',observed); assert participant
    permitted=re.search(r'permitted \|-> (TRUE|FALSE)',observed); assert permitted
    visible=participant[1]=='TRUE' and permitted[1]=='TRUE'
    route=state['route'].strip('"'); cursor=state['cursor'].strip('"') if route=='messages' else 'none'
    cases.add(tuple(boolean(k) for k in ('enabled','activated','exists','blocked'))+
        tuple(has(k,a) for k in ('closed','live','consent') for a in ('a','b'))+
        ({'a':1,'c':3}[state['viewer']],route,cursor,allowed,visible))
lines=['-- Generated from checked DmReads observations; do not hand-edit.','DO $$ BEGIN']
for n,case in enumerate(sorted(cases),1):
    def sql(v):
        if isinstance(v,bool): return 'true' if v else 'false'
        if isinstance(v,str): return "'"+v+"'"
        return str(v)
    lines.append('  PERFORM assert_dm_read_case('+str(n)+','+','.join(map(sql,case))+');')
lines.append('END $$;')
Path(sys.argv[2]).write_text('\n'.join(lines)+'\n')
print(f'Generated {len(cases)} observed cases from {len(nodes)} checked states')

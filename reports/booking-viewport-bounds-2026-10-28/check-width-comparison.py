import argparse,hashlib,json,re
from pathlib import Path
from z3 import Ints,Solver,Not,sat,unsat

def require(ok,message):
    if not ok:raise SystemExit(message)

p=argparse.ArgumentParser();p.add_argument('source',type=Path);a=p.parse_args();data=a.source.read_bytes();text=data.decode()
fields=re.findall(r'overflow:\s*viewport\.scrollWidth\s*>\s*viewport\.(clientWidth|innerWidth)',text)
require(len(fields)==1,'Unsupported or ambiguous comparison syntax')
client,inner,scroll=Ints('client inner scroll');rhs={'clientWidth':client,'innerWidth':inner}[fields[0]]
base=[client>0,client<=inner,scroll>=0];claims=[]
for name,extra in [('no-client-overflow-accepted',[scroll>client,Not(scroll>rhs)]),('old-rejections-retained',[scroll>inner,Not(scroll>rhs)])]:
    s=Solver();s.add(*base,*extra);r=s.check();require(r==unsat,f'{name}: expected unsat, got {r}'+(f' model={s.model()}' if r==sat else ''));claims.append({'id':name,'negation':'unsat','assurance':'PROVEN under stated integer-width abstraction'})
s=Solver();s.add(*base,client==320,inner==400,scroll==400,Not(scroll>inner),scroll>rhs);require(s.check()==sat,'Known mobile false-negative distinction lost')
s=Solver();s.add(client>0,inner>0,scroll>=0,scroll>inner,Not(scroll>client));require(s.check()==sat,'Omitted-ordering control failed to expose necessary assumption')
print(json.dumps({'sourceSha256':hashlib.sha256(data).hexdigest(),'comparisonField':fields[0],'claims':claims,'knownCounterexample':{'client':320,'inner':400,'scroll':400},'omittedOrderingControl':'SAT','assumptions':['Integer widths; positive client; client<=inner; unchanged captured measurement.','Literal field correspondence is hash/syntax binding, not JavaScript/CSSOM/compiler refinement.'],'scope':'Manual comparison abstraction only; no WebKit timing, page-layout or whole-test proof.'},indent=2))

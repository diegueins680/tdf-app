"""Bounded abstract model, not a proof of provider behavior or implementation refinement."""
import itertools,json

def check(ignore_owner=False,ignore_approval=False,retry_unknown=False):
 checked=0;violations=[]
 for owner,actor,tdf,admin,approved,current,dm,dm_approved,state in itertools.product(range(2),range(2),(False,True),(False,True),range(2),range(2),(False,True),(False,True),('queued','claimed','unknown','completed')):
  may_edit=ignore_owner or actor==owner
  authorized=may_edit and (ignore_approval or approved==current) and (not tdf or admin) and (not dm or dm_approved)
  may_dispatch=authorized and (state=='queued' or retry_unknown and state=='unknown')
  checked+=1
  if may_dispatch and actor!=owner:violations.append('cross_tenant')
  if may_dispatch and approved!=current:violations.append('stale_approval')
  if may_dispatch and state=='unknown':violations.append('uncertain_replay')
 return {'states':checked,'violations':sorted(set(violations))}
def main():
 positive=check();controls={'ignore_owner':check(ignore_owner=True),'ignore_approval':check(ignore_approval=True),'retry_unknown':check(retry_unknown=True)}
 if positive['violations'] or not all(v['violations'] for v in controls.values()):raise SystemExit('Model or negative controls failed')
 print(json.dumps({'result':'PASS','positive':positive,'negative_controls':controls,'scope':'Finite abstract dispatch admission only; SQLite interleavings, OAuth and real providers verified separately.'},indent=2))
if __name__=='__main__':main()

import json,tempfile,time,unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from domain import Store,uid
from planning import plan

class PlanningGraphicsTests(unittest.TestCase):
 def setUp(self):
  self.tmp=tempfile.TemporaryDirectory();self.root=Path(self.tmp.name);(self.root/'assets').mkdir();self.store=Store(self.root/'db.sqlite')
  self.actor={'id':1,'name':'Synthetic planner','admin':False};self.store.user(self.actor)
  self.cid=self.store.create(self.actor,{'name':'Synthetic session','objective':'Review a draft'})
  self.source=self.asset('master.mp4');self.old=self.asset('manual.jpg')
  self.job={'id':uid(),'campaign':self.cid,'payload':json.dumps({'source':self.source,'owner':1,'start_at':time.time()+3600,'facts':{}})}
 def tearDown(self):self.tmp.cleanup()
 def asset(self,name):
  aid=self.store.reserve_asset(self.actor,self.cid,name,4)
  with self.store.db(True) as db:db.execute('UPDATE assets SET status="ready",size=4,uploaded=4 WHERE id=?',(aid,))
  return aid
 def piece(self,name,fmt='story',platform='instagram'):
  return self.store.piece(self.actor,self.cid,{'name':name,'format':fmt,'platform':platform,'asset':self.old,'copy':'Manual copy','due_at':time.time()+60,'settings':{'children':[self.old],'custom':'preserve'}})
 def run_plan(self,before_graphics=None):
  graphics={name:self.asset(name+'.jpg') for name in ['S01','S02','S03','S04','S05','S06','S07','S08','CAR01','CAR02']}
  def process(args,**kwargs):
   if args[0]=='ffprobe':return SimpleNamespace(stdout=b'{"format":{"duration":"120"}}')
   output=args[args.index('-af')+1].split('file=',1)[1]
   Path(output).write_text('\n'.join('pts_time:'+str(i*20)+'\nlavfi.astats.Overall.RMS_level=-10' for i in range(6)))
   return SimpleNamespace(stdout=b'')
  def generate(*args):
   if before_graphics:before_graphics()
   return graphics
  with patch('planning.subprocess.run',side_effect=process),patch('media.queue_render',return_value={'job':'synthetic-render'}),patch('graphics.generate',side_effect=generate):
   return plan(self.store,{'STATE_DIR':self.tmp.name},self.job),graphics
 def test_plan_never_overwrites_existing_manual_stories_or_carousels(self):
  story=self.piece('S01');carousel=self.piece('Manual carousel','carousel','facebook')
  before={p['id']:p for p in self.store.detail(self.actor,self.cid)['pieces']}
  result,graphics=self.run_plan();after={p['id']:p for p in self.store.detail(self.actor,self.cid)['pieces']}
  self.assertEqual(after[story],before[story]);self.assertEqual(after[carousel],before[carousel])
  owned=[after[i] for i in result['pieces']]
  self.assertEqual(next(p for p in owned if p['name']=='S01')['asset'],graphics['S01'])
  generated=next(p for p in owned if p['format']=='carousel');settings=json.loads(generated['settings'])
  self.assertEqual(settings['children'],[graphics['CAR01'],graphics['CAR02']]);self.assertTrue(settings['draft_from_template']);self.assertTrue(settings['selection_review_required'])
 def test_late_graphics_do_not_overwrite_an_edited_plan_piece(self):
  edited={}
  def edit():
   row=next(p for p in self.store.detail(self.actor,self.cid)['pieces'] if p['name']=='S01')
   self.store.piece(self.actor,self.cid,{'name':'S01','platform':'instagram','format':'story','asset':self.old,'copy':'Human revision','due_at':row['due_at'],'settings':{'custom':'human'}},row['id'])
   edited.update(next(p for p in self.store.detail(self.actor,self.cid)['pieces'] if p['id']==row['id']))
  self.run_plan(edit)
  after=next(p for p in self.store.detail(self.actor,self.cid)['pieces'] if p['id']==edited['id'])
  self.assertEqual(after,edited)
 def test_late_graphics_do_not_mutate_approved_plan_pieces(self):
  approved={}
  def approve():
   self.store.approve(self.actor,self.cid,{'version':self.store.detail(self.actor,self.cid)['version'],'rights_confirmed':True})
   approved.update({p['id']:p for p in self.store.detail(self.actor,self.cid)['pieces']})
  self.run_plan(approve)
  self.assertEqual({p['id']:p for p in self.store.detail(self.actor,self.cid)['pieces']},approved)

if __name__=='__main__':unittest.main()

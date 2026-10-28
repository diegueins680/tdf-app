import importlib.util,json,pathlib,types,unittest
from unittest.mock import patch
BASE=pathlib.Path(__file__).parent

def module(name,file):
 spec=importlib.util.spec_from_file_location(name,BASE/file);obj=importlib.util.module_from_spec(spec);spec.loader.exec_module(obj);return obj

M=module('private_collector','collect.py');CANONICAL=module('canonical_guard','canonical-inspect-runtime.py')
M.C=CANONICAL;M.SPEC=json.loads((BASE/'query-spec.json').read_text());M.CANONICAL_SHA256='a'*64

def empty(shape):
 if isinstance(shape,dict):return {k:empty(v) for k,v in shape.items()}
 if isinstance(shape,list):return shape[0]
 return shape if type(shape) is bool else 0

class CollectorTests(unittest.TestCase):
 def test_all_new_queries_are_fixed_readonly_role_and_two_second_wrapped(self):
  M.validate_specs()
  for q in [M.SPEC['identity'],M.SPEC['capabilities'],*M.SPEC['queries']]:
   self.assertTrue(q['sql'].startswith(M.PREFIX));self.assertTrue(q['sql'].endswith('ROLLBACK;\n'))
 def test_actual_canonical_command_keeps_socket_role_and_cleared_environment(self):
  c=M.inventory_command('a'*64);self.assertEqual(c[c.index('-U')+1],'tdf_catalog_inventory')
  self.assertEqual(c[c.index('-h')+1],'/var/run/postgresql');self.assertIn('unix:///var/run/docker.sock',c)
  self.assertIn('PGOPTIONS=-c default_transaction_read_only=on -c statement_timeout=2000 -c lock_timeout=500 -c idle_in_transaction_session_timeout=3000',c)
 def test_elevated_query_command_is_rejected_without_fallback(self):
  c=CANONICAL.database_command('a'*64);c[c.index('-U')+1]='postgres'
  with patch.object(CANONICAL,'database_command',return_value=c):
   with self.assertRaises(ValueError):M.inventory_command('a'*64)
 def test_permission_denied_is_unobserved_not_zero_and_raw_error_never_exported(self):
  process=types.SimpleNamespace(returncode=1,stdout='',stderr='ERROR:  42501\nPRIVATE_FAILURE_TEXT')
  with patch.object(M.subprocess,'run',return_value=process) as run:
   got=M.run_query('a'*64,M.SPEC['queries'][0]);self.assertEqual(run.call_args.kwargs['timeout'],8)
  self.assertEqual(got,{'status':'UNOBSERVED','reason':'permission_denied','counts':None,'sqlstate':'42501'})
  self.assertNotIn('PRIVATE_FAILURE_TEXT',json.dumps(got))
 def test_statement_timeout_is_explicitly_unobserved(self):
  p=types.SimpleNamespace(returncode=1,stdout='',stderr='ERROR:  57014\n')
  with patch.object(M.subprocess,'run',return_value=p):got=M.run_query('a'*64,M.SPEC['queries'][0])
  self.assertEqual(got['reason'],'statement_timeout');self.assertIsNone(got['counts'])
 def test_process_timeout_is_explicitly_unobserved(self):
  with patch.object(M.subprocess,'run',side_effect=M.subprocess.TimeoutExpired('psql',8)):
   got=M.run_query('a'*64,M.SPEC['queries'][0])
  self.assertEqual(got['reason'],'process_timeout');self.assertIsNone(got['counts'])
 def test_output_rejects_unknown_strings_extra_keys_bool_or_negative_counts(self):
  for value in [{'value':-1},{'value':True},{'value':1.1},{'value':'private title'},{'value':0,'owner':1}]:
   with self.assertRaises(ValueError):M.validate(value,{'value':0})
  with self.assertRaises(ValueError):M.validate({'state':'private title'},{'state':['ready']})
 def test_authorized_successful_zero_counts_are_observed(self):
  q=M.SPEC['queries'][0];payload=empty(q['shape']);p=types.SimpleNamespace(returncode=0,stdout=json.dumps(payload),stderr='')
  with patch.object(M.subprocess,'run',return_value=p):got=M.run_query('a'*64,q)
  self.assertEqual(got['status'],'OBSERVED');self.assertEqual(got['counts'],payload)
 def test_select_missing_rls_and_missing_relations_do_not_become_zeros(self):
  q=M.SPEC['queries'][0]
  for state in ['permission_denied','rls_coverage_unobserved','relation_missing','unexpected_relation_kind']:
   caps={'status':'OBSERVED','counts':{r:'select_available' for r in M.SPEC['relations']}}
   caps['counts']['music_asset']=state;got=M.blocked_by_capabilities(q,caps)
   self.assertEqual(got['status'],'UNOBSERVED');self.assertIsNone(got['counts']);self.assertEqual(got['relationBlockers']['music_asset'],state)
 def test_unknown_capabilities_block_all_dependent_counts(self):
  self.assertEqual(M.blocked_by_capabilities(M.SPEC['queries'][0],M.unobserved('permission_denied'))['reason'],'relation_capabilities_unobserved')
 def test_identity_unknown_runs_no_business_queries(self):
  before={'db':{'containerId':'a'*64}}
  with patch.object(M,'observe_containers',return_value=before),patch.object(CANONICAL,'verify_storage') as fixed_probe,patch.object(M,'run_query',return_value=M.unobserved('permission_denied')) as query:
   out=M.collect()
  fixed_probe.assert_called_once_with('a'*64);self.assertEqual(query.call_count,1)
  self.assertEqual(out['aggregateFamiliesObserved'],0);self.assertEqual(out['aggregateFamiliesUnobserved'],9)
  self.assertTrue(all(q['counts'] is None for q in out['aggregates'].values()))
 def test_post_collection_identity_change_discards_partial_aggregates(self):
  before={'db':{'containerId':'a'*64}};after={'db':{'containerId':'b'*64}}
  def answer(_db,q):
   data={r:'select_available' for r in M.SPEC['relations']} if q['name']=='relation_capabilities' else empty(q['shape'])
   return {'status':'OBSERVED','counts':data}
  with patch.object(M,'observe_containers',side_effect=[before,after]),patch.object(CANONICAL,'verify_storage'),patch.object(M,'run_query',side_effect=answer):
   with self.assertRaises(ValueError):M.collect()
 def test_success_returns_only_fixed_aggregate_shape_and_identity_fingerprint(self):
  before={'db':{'containerId':'a'*64}}
  def answer(_db,q):return {'status':'OBSERVED','counts':({r:'select_available' for r in M.SPEC['relations']} if q['name']=='relation_capabilities' else empty(q['shape']))}
  with patch.object(M,'observe_containers',return_value=before),patch.object(CANONICAL,'verify_storage'),patch.object(M,'run_query',side_effect=answer):out=M.collect()
  self.assertEqual(out['aggregateFamiliesObserved'],9);self.assertEqual(out['aggregateFamiliesUnobserved'],0)
  self.assertNotIn('containerId',json.dumps(out));self.assertNotIn('a'*64,json.dumps(out['aggregates']))

if __name__=='__main__':unittest.main()

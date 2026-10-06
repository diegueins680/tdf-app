import pathlib,subprocess,re,json,datetime,shutil
P=pathlib.Path('/private/tmp/tdf-branch-audit-20260928');root=pathlib.Path('/Users/diegosaa/.npm/_npx');inspection=json.loads((P/'npm-tool-cache-inspection.json').read_text());allowed={'vercel','expo','wrangler','eas-cli'}
original=json.loads(pathlib.Path('/private/tmp/tdf-disk-cleanup-20260928.json').read_text())
assert P.is_dir() and all(pathlib.Path(x).is_dir() for x in original['existing_worktrees'])
result={'started':datetime.datetime.now(datetime.timezone.utc).isoformat(),'before':subprocess.check_output(['df','-h','/private/tmp'],text=True),'removed':[],'preserved':[]}
for row in inspection['caches']:
 if row['kib']<100000 or row['active_process_reference'] or set(row['package'])-allowed or not row['package']:continue
 p=pathlib.Path(row['directory']);assert p.parent==root and p.resolve().parent==root and not p.is_symlink()
 assert json.loads((p/'package.json').read_text()).get('dependencies',{})==row['package']
 processes=subprocess.check_output(['ps','-axo','command'],text=True)
 handles=subprocess.run(['lsof','-nP','-F','n'],capture_output=True,text=True)
 assert handles.stdout, 'Cannot verify open files'
 if str(p)+'/' in processes+'\n'+handles.stdout:
  result['preserved'].append(str(p));continue
 shutil.rmtree(p);assert not p.exists();result['removed'].append(row);print('Removed inactive CLI cache',row['package'],row['kib'],'KiB',flush=True)
 result['completed']=datetime.datetime.now(datetime.timezone.utc).isoformat();(P/'npm-unused-cli-cleanup.json').write_text(json.dumps(result,indent=2))
assert (root/'51691537fc71f2b0').is_dir(),'Active browser cache must remain'
assert all(pathlib.Path(x).is_dir() for x in original['existing_worktrees']) and P.is_dir()
result['audit_and_all_original_worktrees_preserved']=True;result['active_browser_cache_preserved']=True;result['after']=subprocess.check_output(['df','-h','/private/tmp'],text=True);result['removed_kib']=sum(x['kib'] for x in result['removed']);result['completed']=datetime.datetime.now(datetime.timezone.utc).isoformat();(P/'npm-unused-cli-cleanup.json').write_text(json.dumps(result,indent=2));print(result['after'])

from pathlib import Path
import datetime,hashlib,json,os,subprocess,time
root=Path('/private/tmp/tdf-account-recovery-backend-main624-20261026');out=Path('/private/tmp/tdf-account-recovery-main624-native-20261026');source='2e01bbe1263e8761fa2692bd951f828c01142449'
def git(*args):return subprocess.check_output(['git',*args],cwd=root,text=True).strip()
assert git('rev-parse','HEAD')==source and not git('status','--porcelain','--untracked-files=no')
env={k:v for k,v in os.environ.items() if k in ['HOME','USER','LOGNAME','LANG','LC_CTYPE','PATH','TMPDIR','SHELL']};env['STACK_ROOT']='/Users/macpro/.stack';env['PATH']='/Users/macpro/.ghcup/bin:/private/tmp/tdf-node22-20261026/node-v22.14.0-darwin-x64/bin:/usr/local/opt/postgresql@16/bin:/usr/local/bin:/usr/bin:/bin'
compiler=subprocess.check_output(['stack','--no-terminal','exec','ghc','--','--numeric-version'],cwd=root/'tdf-hq',env=env,text=True).strip();assert compiler=='9.10.3'
command=['stack','--no-terminal','test','--force-dirty','--jobs','2','tdf-hq','--copy-bins','--local-bin-path',str(out/'bin')]
r={'source':source,'backendTree':git('rev-parse',source+':tdf-hq'),'startedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'status':'RUNNING','compiler':compiler,'resolver':'lts-24.42','command':command,'scope':'Fresh canonical Stack compile and aggregate unit tests on immutable protectedmain2e. PostgreSQL/external runners and HTTP journey qualified separately; pending tests are not passes.'}
def save():(out/'receipt.json').write_text(json.dumps(r,indent=2)+'\n')
save()
with (out/'build.log').open('wb') as log:result=subprocess.run(command,cwd=root/'tdf-hq',env=env,stdout=log,stderr=subprocess.STDOUT)
r['exitCode']=result.returncode;r['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();r['status']='PASS' if result.returncode==0 else 'FAILED'
if result.returncode==0:
 p=out/'bin/tdf-hq-exe';assert p.exists();r['binary']=str(p);r['binarySha256']=hashlib.sha256(p.read_bytes()).hexdigest();assert git('rev-parse','HEAD')==source and not git('status','--porcelain','--untracked-files=no')
save();print(json.dumps(r,indent=2));raise SystemExit(result.returncode)

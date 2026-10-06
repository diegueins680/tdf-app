import os,pathlib,subprocess,time,sys
P=pathlib.Path(__file__).parent;cwd=P/'ingestion/tdf-hq'
# Wait for the already-running invocation before reusing its interface cache.
while True:
 try:os.kill(22010,0)
 except ProcessLookupError:break
 time.sleep(2)
dist=subprocess.check_output(['stack','path','--dist-dir'],cwd=cwd,text=True).strip()
cmd=['stack','exec','--','ghc','-O0','-Wall','-fno-code','-fwrite-interface','-isrc','-itest','-i'+dist+'/build/tdf-hq-exe/autogen','-outputdir','.stack-work/event-raci-browser','src/TDF/Cron.hs']
with open(P/'pr463-cron-typecheck-final.log','w') as log:
 r=subprocess.run(cmd,cwd=cwd,stdout=log,stderr=subprocess.STDOUT)
(P/'pr463-cron-verified-exit.txt').write_text(str(r.returncode)+'\n')
print('Final Cron typecheck exit',r.returncode,flush=True)
sys.exit(r.returncode)

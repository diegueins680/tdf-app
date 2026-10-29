from pathlib import Path
import subprocess,os,socket,time,urllib.request,json,signal
root=Path('/private/tmp/tdf-booking-tooltip-regression-qualified-v3-20261028')
work=Path('/private/tmp/tdf-provider-evidence-actualmain-20261028')
cache=Path('/private/tmp/tdf-correctness-cycle-20261028')
created=[];preview=None;browser=None;record={'executionContext':'main session normal tools; no permission override','startedAt':time.time(),'ownedAliases':[],'browserExit':None}
def stop(proc):
    if proc and proc.poll() is None:
        os.killpg(proc.pid,signal.SIGTERM)
        try:proc.wait(timeout=8)
        except subprocess.TimeoutExpired:
            os.killpg(proc.pid,signal.SIGKILL);proc.wait(timeout=5)
try:
    with socket.socket() as s:s.bind(('127.0.0.1',41111))
    for name in ['node_modules','tdf-hq-ui/node_modules']:
        target=work/name;source=cache/name
        if target.exists() or target.is_symlink():
            if target.resolve()!=source.resolve():raise RuntimeError('Unexpected dependency alias '+str(target))
        else:
            target.symlink_to(source,target_is_directory=True);created.append((target,source));record['ownedAliases'].append(str(target))
    with (root/'preview-positive-initial-phone.log').open('w') as serverlog:
        preview=subprocess.Popen(['node',str(cache/'node_modules/vite/bin/vite.js'),'preview','--host','127.0.0.1','--port','41111','--strictPort'],cwd=work/'tdf-hq-ui',stdout=serverlog,stderr=subprocess.STDOUT,start_new_session=True)
        record['previewPid']=preview.pid;print('Owned preview PID',preview.pid,flush=True)
        for _ in range(100):
            if preview.poll() is not None:raise RuntimeError('Preview exited early')
            try:
                with urllib.request.urlopen('http://127.0.0.1:41111',timeout=1) as r:
                    if r.status==200:break
            except OSError:time.sleep(.1)
        else:raise RuntimeError('Preview did not become ready')
        with (root/'run-positive-initial-phone.log').open('w') as log:
            browser=subprocess.Popen(['node',str(cache/'node_modules/@playwright/test/cli.js'),'test','--config',str(root/'playwright.phone.config.mjs'),'booking.spec.mjs','--grep','recovers an error'],cwd=root,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
            record['browserPid']=browser.pid;print('One permanent-regression initial phone positive case running, PID',browser.pid,flush=True)
            try:record['browserExit']=browser.wait(timeout=120)
            except subprocess.TimeoutExpired:
                record['timeout']=True;stop(browser);record['browserExit']=browser.returncode
            print('Browser exit',record['browserExit'],flush=True)
finally:
    stop(browser);stop(preview)
    for target,source in reversed(created):
        if target.is_symlink() and target.resolve()==source.resolve():target.unlink()
        else:record.setdefault('cleanupWarnings',[]).append(str(target))
    record['finishedAt']=time.time();record['previewStopped']=preview is None or preview.poll() is not None
    record['ownedAliasesRemoved']=all(not p.is_symlink() for p,_ in created)
    (root/'execution-positive-initial-phone.json').write_text(json.dumps(record,indent=2)+'\n')
    print('Owned preview stopped; own aliases removed',flush=True)

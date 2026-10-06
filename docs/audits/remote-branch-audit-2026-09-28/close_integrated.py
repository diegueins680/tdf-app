import datetime,json,pathlib,subprocess,os
P=pathlib.Path(__file__).parent
E={k:v for k,v in os.environ.items() if k not in ('GH_TOKEN','GITHUB_TOKEN','GITHUB_PAT')}
R='repos/diegueins680/tdf-app'
def api(path,method='GET',body=None):
    c=['/usr/local/bin/gh','api',path,'--method',method]
    if body is not None:c+=['--input','-']
    r=subprocess.run(c,input=json.dumps(body) if body is not None else None,text=True,capture_output=True,env=E)
    if r.returncode:raise RuntimeError(r.stderr)
    return json.loads(r.stdout) if r.stdout.strip() else None
def record(data):
    with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**data})+'\n')
for n in [415,409,402,397,394,391,390]:
    initial=json.loads((P/f'prs/{n}/info.json').read_text())
    current=api(f'{R}/pulls/{n}')
    if current['state']!='open':
        record({'action':'closure_skipped_concurrent','pr':n,'state':current['state'],'merged':current['merged']});continue
    assert current['head']['sha']==initial['head']['sha'] and current['base']['ref']==initial['base']['ref'],f'Concurrent PR change {n}'
    successor=api(f'{R}/pulls/424');assert successor['merged']
    main=api(f'{R}/branches/main')['commit']['sha']
    sha=current['head']['sha']
    comparison=api(f'{R}/compare/{sha}...{main}')
    assert comparison['behind_by']==0 and comparison['merge_base_commit']['sha']==sha
    replacement=api(f'{R}/compare/{sha}...{successor["merge_commit_sha"]}')
    assert replacement['behind_by']==0 and replacement['merge_base_commit']['sha']==sha
    evidence={'pr':n,'before':current,'target':main,'replacement':successor['merge_commit_sha'],'comparison':{'ahead_by':comparison['ahead_by'],'behind_by':comparison['behind_by'],'merge_base':comparison['merge_base_commit']['sha']}}
    (P/f'closure-{n}-evidence.json').write_text(json.dumps(evidence,indent=2))
    body=f'''Classification: ALREADY_MERGED (high confidence).

Merged replacement #424 ({successor['html_url']}) integrated this branch and preserved its source commits and contributor attribution. Fresh GitHub comparisons confirm that the complete current head `{sha}` is an ancestor of both replacement merge `{successor['merge_commit_sha']}` and current `main` `{main}` (zero head-only commits).

No additional rescue is required: the full branch history is already integrated. Closing this duplicate integration request; this does not authorize social-enforcement activation or close any linked issue. The remote branch is retained pending the separate operational/deployment/dependency deletion safeguards.

Recovery head: `{sha}`. Branch: `{current['head']['ref']}`.'''
    # Re-read directly before the externally visible comment and state mutation.
    fresh=api(f'{R}/pulls/{n}')
    assert fresh['state']=='open' and fresh['head']['sha']==sha and fresh['base']['ref']==current['base']['ref']
    comment=api(f'{R}/issues/{n}/comments','POST',{'body':body})
    record({'action':'classification_comment','pr':n,'sha':sha,'url':comment['html_url']})
    fresh=api(f'{R}/pulls/{n}')
    assert fresh['state']=='open' and fresh['head']['sha']==sha and fresh['base']['ref']==current['base']['ref']
    api(f'{R}/pulls/{n}','PATCH',{'state':'closed'})
    result=api(f'{R}/pulls/{n}')
    assert result['state']=='closed' and not result['merged'] and result['head']['sha']==sha
    record({'action':'closed_unmerged','pr':n,'sha':sha,'url':result['html_url'],'comment':comment['html_url'],'replacement':424})
    print('Verified closed',n,flush=True)

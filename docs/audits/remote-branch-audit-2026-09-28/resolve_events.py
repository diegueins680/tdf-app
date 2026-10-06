import pathlib,re,subprocess,json
ROOT=pathlib.Path(__file__).parent/'events'
pattern=re.compile(r'^<<<<<<< .*?\n(.*?)^=======\n(.*?)^>>>>>>> .*?\n',re.M|re.S)
def resolve(name,choices):
    p=ROOT/name;i=iter(choices)
    def sub(m):
        c=next(i);a,b=m.groups()
        return a if c=='ours' else b if c=='theirs' else a+b if c=='both' else c(a,b)
    p.write_text(pattern.sub(sub,p.read_text()))
def content(ref,name):return subprocess.check_output(['git','show',ref+':'+name],cwd=ROOT,text=True)
resolve('.github/workflows/ci.yml',['both'])
resolve('.github/workflows/event-operations-formal.yml',['ours','ours'])
resolve('e2e/web/fanhub-onboarding.spec.mjs',['ours','ours'])
resolve('formal/event-operations/FanHubOnboarding.tla',['ours'])
resolve('scripts/__tests__/catalog-list-audit.test.mjs',['both'])
resolve('scripts/__tests__/ci-pipeline.test.mjs',[lambda a,b:a+'});\n\n'+b])
resolve('scripts/__tests__/event-operations-pluscal.test.mjs',['ours'])
resolve('scripts/__tests__/production-release.test.mjs',['ours'])
# Main already includes canonical migration ordering and ancestry repairs. New
# event adapters deliberately remain outside automatic activation/registration.
(ROOT/'scripts/production-migrations.json').write_text(content('HEAD','scripts/production-migrations.json'))
resolve('scripts/test-event-operations-foundation-migration.sh',['ours'])
def runners(a,b):
    # Preserve current liveness checks and add task-specific checks. The same
    # FanHub negative controls already run under current runner semantics.
    b=b[:b.index('run_tlc FanHubOnboarding.tla')]
    b=b.replace('run_tlc OperationalLiveness.tla OperationalLiveness.cfg operational-liveness\n','')
    return a+b
resolve('scripts/verify-event-operations-formal.sh',['ours',runners])
resolve('tdf-hq-ui/src/analytics/onboardingProgress.ts',['ours','ours'])
resolve('tdf-hq-ui/src/analytics/onboardingProgress.test.ts',['both'])
resolve('tdf-hq-ui/src/pages/ArtistPublicPage.component.test.tsx',['theirs'])
resolve('tdf-hq-ui/src/pages/DirectoryPublicDetailPage.test.tsx',['theirs'])
resolve('tdf-hq-ui/src/pages/DirectoryPublicDetailPage.tsx',['ours','theirs','theirs'])
resolve('tdf-hq-ui/src/pages/FanHubPage.onboarding.test.tsx',['theirs','theirs','theirs','theirs','theirs'])
resolve('tdf-hq-ui/src/pages/FanHubPage.tsx',['ours','theirs','ours','theirs','theirs',lambda a,b:b.replace('<Typography variant="subtitle1" fontWeight={700}>','<Typography variant="subtitle1" fontWeight={700} color="text.primary">')])
resolve('tdf-hq-ui/src/pages/LoginPage.test.tsx',['theirs'])
resolve('tdf-hq-ui/src/pages/LoginPage.tsx',['theirs'])
resolve('tdf-hq-ui/src/routes/AppShell.test.tsx',['ours'])
# Current main moved this recovery to the session owner, including public routes.
resolve('tdf-hq-ui/src/routes/AppShell.tsx',['ours','ours'])
resolve('tdf-hq-ui/src/session/SessionProvider.personalData.test.tsx',['theirs'])
resolve('tdf-hq/docs/openapi/api.yaml',['both','both'])
resolve('tdf-hq/src/TDF/Auth.hs',['both','both','theirs',lambda a,b:a.replace('tokenKey','tokenId')+b])
resolve('tdf-hq/src/TDF/Server/SocialEventsHandlers.hs',['ours','ours'])
for p in (ROOT/'tdf-hq/test').rglob('*.hs'):
    s=p.read_text()
    if pattern.search(s):
        assert all('auApiTokenId' in m[0] and 'auSessionWitness' in m[1] or p.name=='Spec.hs' for m in pattern.findall(s)),p
        p.write_text(pattern.sub(lambda m:m[1]+m[2],s))
# Both proof-carrying authentication and the existing social token identity must
# remain available. Complete constructors introduced independently on each side.
for folder in ['tdf-hq/src','tdf-hq/test']:
    for p in (ROOT/folder).rglob('*.hs'):
        s=p.read_text()
        s=re.sub(r'(?m)^(\s*), auApiTokenId = Nothing\n(?!\s*, auSessionWitness)',r'\1, auApiTokenId = Nothing\n\1, auSessionWitness = Nothing\n',s)
        s=re.sub(r'(?m)^(\s*), auSessionWitness = Nothing\n',lambda m: m[0] if 'auApiTokenId' in s[max(0,m.start()-100):m.start()] else m[1]+', auApiTokenId = Nothing\n'+m[0],s)
        p.write_text(s)

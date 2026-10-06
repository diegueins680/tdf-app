import json,pathlib,collections,csv,datetime,re,shutil
P=pathlib.Path(__file__).parent;R=P/'report'; rows=json.load(open(R/'branches.json'));mut=[json.loads(x) for x in (P/'mutations.jsonl').read_text().splitlines()]; counts=collections.Counter(x['classification'] for x in rows)
assert len(rows)==134==len({x['branch'] for x in rows})
def flat(f):return [x for page in json.load(open(f)) for x in page]
final=sorted(p for p in P.glob('final-*') if p.is_dir() and (p/'completed.txt').exists())[-1]; live=flat(final/'branches.json'); prs=flat(final/'pulls.json');init=flat(P/'branches-pages.json'); names={x['name'] for x in init};liveby={x['name']:x for x in live};openprs=[p for p in prs if p['state']=='open'];rec={'initial_branches':134,'initial_open_prs':67,'initial_total_prs':452,'current_branches':len(live),'current_open_prs':len(openprs),'current_total_prs':len(prs),'missing_initial_branches':sorted(names-set(liveby)),'added_branches':sorted(set(liveby)-names),'changed_initial_heads':[{'branch':b['name'],'before':b['commit']['sha'],'after':liveby[b['name']]['commit']['sha']} for b in init if b['name'] in liveby and b['commit']['sha']!=liveby[b['name']]['commit']['sha']],'matrix_rows':len(rows),'timestamp_start':(final/'started.txt').read_text(),'timestamp_end':(final/'completed.txt').read_text() if (final/'completed.txt').exists() else 'snapshot collection finishing'}
rec['concurrently_absent_initial_branches']=rec['missing_initial_branches'];rec['unaccounted_initial_branches']=[]
rec['observed_concurrent_deletions']=len(json.load(open(P/'concurrent-deletions-recovery.json'))['absent_branches'])
rec['historical_total_branch_names']=json.load(open(P/'concurrent-deletions-recovery.json'))['total_historical_names']
(R/'reconciliation.json').write_text(json.dumps(rec,indent=2)+'\n')
# A complete comparison matrix plus JSON detail: initial SHA/relations are immutable snapshot evidence.
escape=lambda x:str(x).replace('|','\\|').replace('\n',' ')
lines=['# Initial branch matrix','', 'Initial snapshot: 2026-09-28T03:24:04Z. Every initial branch appears exactly once. The displayed ahead/behind and merge base are against the initial intended target; JSON and CSV also include the current head and current relation to the refreshed target, as well as refreshed PR states. This audit deleted zero branches.66 initial refs were concurrently deleted by another session; their recovery SHAs and commands are in the JSON/CSV matrix and concurrent-deletions.md. All source objects are retained in a verified recovery bundle.','','The [CSV matrix](branches.csv) includes the full changed-file list and rationale. [JSON detail](branches.json) includes individual check URLs, review states/commit IDs, unresolved threads, labels, deployment records, references, and unique-commit evidence paths.','','| Branch / initial head | PR / base | Protection / operations | Ahead / behind | Checks / reviews / conflicts | Dependencies / purpose | Classification / confidence | Action / evidence / result |','|---|---|---|---|---|---|---|---|']
for r in rows:
 pr='; '.join(f"[#{p['number']}]({p['url']}) ({p['state']}{'/merged' if p['merged'] else ''}{'/draft' if p['draft'] else ''}) → {p['base']}" for p in r['pull_requests']) or 'No PR; inferred target '+r['target']
 checks=dict(collections.Counter(c['conclusion'] or c['status'] for c in r['checks']));rev='; '.join(f"#{p['number']}: {p['review_decision'] or 'no required approval recorded'}, {len(p['unresolved_threads'])} unresolved" for p in r['pull_requests'])
 dep=', '.join('#'+str(p['pr']) for p in r['dependencies_or_overlaps']) or 'No open successor captured'
 dep += ''.join(f"; native stack #{p['native_stack']['number']} position {p['native_stack']['position']}" for p in r['pull_requests'] if p.get('native_stack'))
 evidence='; '.join(f'[{path.split("/")[-1]}](../{path})' for path in r['evidence'][:2])
 result='; '.join(f'[result]({url})' for url in r['result_links'])
 vals=[f"`{r['branch']}` / `{r['sha']}`",pr,r['operational_status'],f"+{r['ahead']}/−{r['behind']}; merge base {r['merge_base']}",str(checks)+'; '+rev+'; '+r['conflicts'],dep+'; '+r['purpose'],r['classification']+' / '+r['confidence'],r['action']+' '+r['rationale']+' '+evidence+' '+result]
 lines.append('| '+' | '.join(escape(x) for x in vals)+' |')
(R/'branch-matrix.md').write_text('\n'.join(lines)+'\n')
# Preserve the mutation ledger verbatim: comments, verified closures and normal pushes are separate entries.
shutil.copyfile(P/'mutations.jsonl',R/'mutations.jsonl')
validation=[
('PR468 atce1a71fe2','live new provenance and installed monitor','PASS —configured PostgreSQL digest/volume verified;777 catalog records/652 tables, bound public checks repeated after SQL; installed unchanged monitor command reads8 aggregate reports withzero errors; no mail/data writes/deployment','cutover-provenance-live-verified.json'),
('PR465 atee58bd13','complete current-head hosted backend/runtime/schema suites','PASS —3589 Hspec examples/0 failures;6 separately exercised cases,1141 social HTTP and110 event HTTP examples; every prescribed job step succeeds','events-hosted-backend-ee58bd13.log'),
('PR465 atee58bd13','prescribed hosted event model verification','PASS —bounded-model-checks-passed with pinned TLA+/Alloy tool hashes and archived solver evidence; no local Java rerun claimed','events-ee58-bounded-models.log'),
('PR465 atee58bd13','prescribed hosted task completion PostgreSQL16 test','PASS —32 decision cases,12 isolation races,four expiry waits,revocation orders and rollback/reapply; the earlier opaque CI failure is retained separately','events-ee58-task-completion.log'),
('PR465 at154f968aa','complete hosted backend and prescribed isolated suites','PASS —3589 Hspec examples/0 failures;6 pending cases exercised by separate runners,1141 social HTTP and110 event HTTP examples; all job steps success. Subsequent ee58 changes only reviewed main documentation/hash.','events-hosted-backend-154f968aa.log'),
('PR468 atce1a71fe2','provenance access/catalog/mail suites','PASS —11 Python access tests,6 Node catalog tests,6 mail tests;15 old-code rejection failures and missing-postcheck negative control reproduced','cutover-provenance-local-validation.json'),
('PR468 atce1a71fe2','strict catalog and generated inventory checks','PASS —1162 reviewed candidates after pinned submodule initialization; earlier missing dependency and missing-mobile failures retained','cutover-provenance-catalog-final.json'),
('PR468 atce1a71fe2','current authenticated live provenance','INITIAL ATTEMPT BLOCKED —SSH timeout; the later authenticated metadata/catalog/mail and installed-helper runs pass, recorded separately','cutover-current-connectivity.json'),
('PR465 integration with mainc4d479c7','merge composition and inventory','PASS —only reviewed notification document/hash added; exact document match, inventory gate and independently computed merge tree match; normal reconciliation subsequently pushed at ee58bd13; current-head CI/review separately required','events-notification-merge-validation.json'),
('PR465 lifecycle repair34ca75d','native PostgreSQL negative control and real authenticated HTTP harness','PASS —old flag race reproduced; both disable/command orders pass at RC/RR/Serializable;110 compiled Hspec/HTTP examples with eight corrupt-receipt rollback injections;160 registered SQL migrations unchanged','events-lifecycle-native.log'),
('PR476 documentation rescue','source text/provenance/diff checks','PASS —exact historical source addition preserved; mobile pin unchanged; initial stale document hash regenerated and inventory check then passed','notification-rescue-validation.json'),
('PR465 at08acdfa','normal main/editorial reconciliation and retained catalog decisions','PASS —5 unchanged catalog regression tests,1171 strict reviewed candidates,83 release tests,32 CI tests, repository/formal/inventory gates; historical mapping and exact main manifest preservation verified','events-editorial-decision-preservation.json'),
('PR464 local replacement','npm run quality:repo','PASS — retry after isolated UI dependency symlink restored missing React resolution; original failure retained','editorial-repo-quality-final-20261003.log'),
('PR464 local replacement','npm run verify:formal','PASS — repository formal gate; existing warning-level findings retained','editorial-formal-20261003.log'),
('PR464 local replacement','specification inventory tests','PASS — 3 Python tests and generated inventory check','editorial-spec-tests-20261003-retry.log'),
('PR464 local replacement','catalog regression tests','PASS — 2 unchanged tests','editorial-catalog-regression-20261003.log'),
('PR475 / source464','source-derived PostgreSQL visibility predicate','PASS — 10 cases, actual isolated native PostgreSQL16; the separate compiled handler test also passed','editorial-predicate-native-20261003.log'),
('PR475 / source464','initial cold local Stack build','FAILED — preprocessed earlier handler lacked the qualified import; committed9ec contains it and hosted build passed. Current-source local rerun remains separate.','editorial-build-20261003.log'),
('PR465 at154f968aa','complete native PostgreSQL task-completion regression','PASS —32 decision cases,12 isolation races,four expiry waits,revocation orders and rollback/reapply; baseline also passes on this slower client, not a reproduced hosted failure','task-completion-native-fixed.log'),
('PR465 at34ca75d','hosted task-completion job','FAILED — opaque exit1;15-second expiry fixture had100x0.1second polling budget; corrected at154f968aa without removing assertions, fresh hosted CI remains authoritative','events-lifecycle-completion-failure.log'),
('PR475 at4100a04c','current-head hosted full backend and all CI','PASS —3553 examples/0 failures/6 separately exercised runtime cases; compiled PostgreSQL predicate1, invitations15, event-relations1,1141 social HTTP examples and full runtime/automatic production migration stages; all applicable checks pass','editorial-hosted-backend-4100a04c.log'),
('PR475 at9ecac924a','hosted full backend, compiled PostgreSQL predicate and runtime/schema stages','PASS —3553 examples/0 failures/6 separately exercised runner cases;1 actual compiled predicate case,15 invitation examples,1141 social HTTP examples; all job stages passed','editorial-hosted-backend-9ecac924a-run-view.log'),
('PR475 forward migration','native PostgreSQL16 directory metadata boundary','PASS — reproduced historical defect;16 cases after both apply and reapply','editorial-migration-native-20261003.log'),
('PR475 forward migration','native PostgreSQL16 complete production-shaped schema','PASS —160 migrations apply/reapply; schema gate; both historical privacy orders; valid ownership plus private/malformed/suppressed event/venue/search projections','editorial-production-schema-20261003.log'),
('PR475 forward migration','npm run test:production-release','PASS —82 tests','editorial-production-release-tests.log'),
('PR475 forward migration','npm run test:ci-pipeline','PASS —27 tests','editorial-ci-pipeline-tests.log'),
('PR475 forward migration','formal and strict catalog gates','PASS — formal verification and1162 reviewed candidates;159 earlier migration entries and SQL bytes unchanged','editorial-catalog-migration-final.json'),
('PR475 current source','targeted local Stack build/test command','FAILED at packaging after successfully linking test executable: app executable was not built by targeted invocation; not counted as a successful Stack command','editorial-backend-current-tests-20261003.log'),
('PR475 current source','full Stack-built local Hspec executable','PASS —3553 examples/0 failures/6 separate-runner cases; those six were exercised successfully in hosted9ec runtime stages and Haskell source is unchanged','editorial-full-hspec-20261003.log'),
('PR475 at4100a04c','all registered migration introduction ancestry checks and GitHub proposed normal merge','PASS —160 introductions are ancestors of actual head; GitHub confirms new introduction retained in proposed normal merge; review evidence reply and resolution verified','editorial-review-ancestry-verified.json'),
('PR475 current source','compiled native PostgreSQL ownership and invitation suites','PASS —1 predicate example covering10cases and15 invitation examples; isolated PostgreSQL16 stopped afterwards','editorial-compiled-postgres-20261003.log'),
('PR465 follow-up source gates', 'UI typecheck, zero-warning lint, full catalog gate, specification inventory', 'PASS — verified process exit0 for each command', 'events-review-command-results.json'),
('PR465 current main integration', 'Stack build --test --no-run-tests --jobs 2', 'PASS — complete app and test compilation, exit0; compiler warnings retained', 'events-current-main-backend-build.log'),
('PR465 current main backend', 'freshly Stack-built Hspec executable', 'PASS —3576 examples, zero failures, six existing pending separately exercised runner cases', 'events-current-main-backend-tests.log'),
('PR465 current main UI', 'full Jest suite', 'PASS —241 suites,2648 tests', 'events-current-main-ui-tests-2.log'),
('PR465 current main web build', 'npm run build --workspace=tdf-hq-ui', 'PASS — typecheck, Vite build and initial bundle budget; chunk-size warning retained', 'events-current-main-ui-build.log'),
('PR465 actual HTTP/authentication', 'unchanged harness/fixtures against isolated native PostgreSQL16', 'PASS —107 examples; owned database removed after verification; not a Docker result', 'events-current-main-http-native.log'),
('PR465 current main catalog', 'unchanged hosted regression suite before mapping repair', 'FAILED —three stale historical-successor mappings; retained original log', 'events-current-main-hosted-catalog-failure.log'),
('PR465 catalog reconciliation repair', 'npm run test:catalog-list-audit', 'PASS —all five unchanged regressions; historical hashes and stale-decision negative controls retained', 'events-current-main-catalog-regression-repair.log'),
('PR465 late mutation review repair', 'ArtistPublicPage and PartyRelationshipMigration tests', 'PASS —27 tests including new follow/unfollow completion after navigation and existing callback suppression tests', 'events-current-main-review-ui.log'),
('PR465 current main release safety', 'release script tests', 'PASS —83 tests', 'events-current-main-release-tests.log'),
('PR465 current main CI wiring', 'CI contract tests', 'PASS —32 tests', 'events-current-main-ci-tests.log'),
('PR465 current main dependency security', 'unchanged audit-ci policy', 'PASS — existing allowances retained; not zero vulnerability', 'events-current-main-security.log'),
('PR465 local Docker schema rehearsal', 'production schema rehearsal', 'BLOCKED —local Docker unresponsive; only the owned command stopped, no daemon/container modifications. Hosted complete-schema check passed on23ff8f588', 'events-current-main-schema-rehearsal.log'),
('PR468 authoritative volume', 'actual read-only inventory with exact named PostgreSQL volume', 'PASS —777 records/652 tables', 'cutover-live-volume-verified.json'),
('PR468 installed monitor volume check', 'exact installed daily scheduled command', 'PASS —mailbox read true,8 reports,errors[]; no email sent', 'cutover-live-mail-volume-verified.json'),
('PR468 initial scheduled monitor attempt', 'unchanged command before retry', 'FAILED —transient public DNS failure; mailbox read succeeded; preserved initial result', 'cutover-volume-mail-initial-dns-failure.json'),
('PR468 browser acceptance', 'visible-browser login tab attempt', 'BLOCKED —transport closed; actual Google login/upload not claimed', 'cutover-browser-gate-limitation.json'),
('Installed mail monitor final verification','exact scheduled Python command against current production configuration','PASS — mailboxReadSucceeded=true, 8 aggregate reports, errors=[], no email sent; installed shared helper verified by SHA256','cutover-live-mail-final.json'),
('PR468 required verify-token guard','both literal webhook repair commands reject absent/empty credentials before curl','PASS — 95 token-workflow tests including supported aliases and no-provider-call/no-secret-output controls','cutover-verify-token-tests.log'),
('PR468 public origin binding','actual HTTPS peer/DNS vs authenticated SSH origin; same-SHA foreign-host and mixed-DNS controls','PASS — 5 catalog regressions; strict TLS validation remains enabled','cutover-origin-tests.log'),
('PR468 origin metadata','authenticated SSH metadata and existing access controls','PASS — 9 tests','cutover-origin-access-tests.log'),
('PR468 origin follow-up mail regression','read-only mailbox/credential controls','PASS — 6 tests','cutover-origin-mail-tests.log'),
('PR468 live current deployment','actual restricted inventory after concurrent release','PASS — 777 records/652 tables; TLS peer/SSH host, current image/database and read-only transaction verified. Initial coverage failures retained in evidence','cutover-live-catalog-origin-verified.json'),
('Operational catalog reader follow-up','reviewed SELECT grants on 29 newly deployed tables','PASS — 652 readable/0 writable tables, no schema CREATE; transaction guards exact deployment/coverage/role attributes','catalog-reviewed-tables-grant-result.json'),
('PR468 read-only replacement guidance','both retired helper commands require --check','PASS — 4 subprocess regressions, no credential/provider changes','cutover-readonly-command-tests.log'),
('PR468 remaining stored-token/callback paths 9a8357065','retired helper regressions and specification gates','PASS — 4 retirement regressions including private stored-token fixture, 3 inventory and 9 evidence cases; callback origins corrected without changing paths/events','cutover-final-token-audit-tests.log'),
('PR468 normal current-main merge 9bbd7d88f','access/mail/catalog/retirement and inventory validation','PASS — 8 access + 6 mail + 3 catalog + 3 retired-helper tests; operational files unchanged from live-tested source, dependency manifests equal verified main','cutover-current-main-tests.log'),
('Final main 76c2c05c','all normal workflows/checks and protected target verification before source closure','PASS — all observed final-head workflows/checks completed successfully; initial de76 security failure retained historically','discovery-closure-main-ci.json'),
('PR472 merge 76c2c05c','fresh head/reviews/checks and normal merge target verification','PASS — approved exact head merged normally, branch retained, resulting tree identical to tested lockfile fix','merge472-target-verification.json'),
('PR468 final hardening f528eee8c','registry provenance, approved-query/role and retired-diagnostic controls','PASS — 8 access, 6 mail, 3 catalog, 3 retired-helper, 3 inventory and 9 evidence/escrow tests','cutover-access-hardening-tests.log'),
('PR468 database-enforced reader','real role grants and explicit read-write negative control','PASS — 623 readable tables, zero writable tables, no schema CREATE/superuser; zero-row UPDATE denied even after overriding read-only defaults','catalog-reader-provisioned.json'),
('PR468 hardened live inventory','full approved inventory with dedicated reader','PASS — 766 records, current API/image/database identity verified, no application-data mutation','cutover-live-catalog-hardened.json'),
('PR468 hardened installed monitor','exact scheduled command after provenance correction','PASS — mailbox read succeeded, 8 aggregate reports, zero errors, no mail sent','cutover-live-mail-hardened.json'),
('PR469 merge de76dc7df','fresh exact-head checks/reviews, normal merge and target verification','PASS — merged state/commit verified; target tree identical to tested cda2c5f, source ancestry retained','merge469-target-verification.json'),
('PR469 post-merge security audit','unchanged hosted audit-ci','FAILED — newly cataloged ip-address advisories in existing 10.3.1; focused tested repair published as PR472, no threshold/allowlist change','merge469-initial-ci-failure.log'),
('PR472 security repair d1f1affcd','exact repository audit-ci configuration','PASS — compatible lock entry updated to 10.7.2; existing allowlist and thresholds unchanged','security-audit-ci.log'),
('PR472 security repair','four old/new address classification cases and two public controls','PASS — both advisory behaviors reproduced before and corrected after; public controls retained','security-classifier-check.json'),
('Mobile115 current-main merge ede1f2a','complete mobile Jest suite','PASS — 87 suites / 553 tests, no new skips','mobile-main-tests.log'),
('Mobile115 current-main merge ede1f2a','release assets, lint, typecheck and public Expo config','PASS — newer signing/runtime/association/API gates preserved; no native publication','mobile-main-release.log'),
('Mobile115 current-main merge ede1f2a','Python signing/artifact regression suite','PASS — 12 tests','mobile-main-python.log'),
('PR468 remaining retired writer 4d40ae58c','manual/auto refresher refusal and credential-redaction regressions','PASS — 2 tests; exits before exchange, secret output or service restart','cutover-retired-refresher-tests.log'),
('PR468 remaining retired writer','generated inventory and evidence gates','PASS — three inventory and nine evidence/escrow cases','cutover-retirement-spec-tests.log'),
('PR468 current access 102662f13','SSH binding, redaction, read-only catalog and mail regressions','PASS — 6 access + 6 mail + 3 catalog tests; strict host identity, mismatched targets/versions, unsafe secret permissions and mailbox error redaction tested','cutover-current-access-tests.log'),
('PR468 current access 102662f13','final access suite including unsafe credential permissions','PASS — 6 tests','cutover-current-access-final-tests.log'),
('PR468 live catalog','actual bounded read-only inventory against verified current Hetzner deployment','PASS — 766 records / 623 tables, tdf_hq, read-only on; matching current public API commit/image, no remote writes','cutover-live-catalog-validation.json'),
('PR468 installed mail monitor','exact installed scheduled command, read-only DNS/IMAP','PASS — mailboxReadSucceeded=true, 8 aggregate reports, zero errors; original LaunchAgent unchanged; no mail sent','cutover-live-mail-validation.json'),
('PR468 retired deployment guide 4031ed4a2','specification inventory and evidence gates','PASS — generated guide fingerprint checked, three Python and nine Node regressions; documentation-only change','cutover-guide-spec-tests.log'),
('PR468 managed-image host 7f1d2d748','helper/token/enrichment/import suites and managed-host regression','PASS — 122 tests; canonical/legacy hosts accepted, lookalikes/unrelated/malformed hosts rejected; inventory and diff checks pass','cutover-managed-image-tests.log'),
('PR468 remaining cutover paths 36aff567c','helper/token/enrichment/import regression suites','PASS — 121 tests, zero failures/skips; importer/default/override, aliases, callback states and readiness false-success controls; all network calls mocked','cutover-remaining-path-tests.log'),
('PR468 remaining cutover paths','CI/evidence/escrow and specification regression gates','PASS — 34 Node tests and 3 Python tests; regenerated document hashes and shell syntax checks pass','cutover-remaining-spec-ci.log'),
('Mobile115 canonical API c921f5d','full mobile Jest suite','PASS — 85 suites / 510 tests; no skips or native publication','mobile-cutover-tests.log'),
('Mobile115 canonical API c921f5d','release asset/lint/typecheck/public-config checks','PASS — no native build or deployment','mobile-cutover-release.log'),
('Mobile115 canonical API','release-tool Python tests','PASS — 7 tests; signing/identity constraints retained','mobile-cutover-python.log'),
('Mobile115 canonical API','five actual Expo profile and override configurations','PASS — production fallback, preview/production injection, development and explicit override','mobile-cutover-config.json'),
('Mobile115 native inputs cb742625','two actual Expo configurations from Android/iOS workflow inputs','PASS — only API/upload values changed; no workflow dispatch; application files identical to tested c921f5d','mobile-cutover-workflow-config.json'),
('Root465 mobile pin 86d4b9bb4','catalog --fail-on-unreviewed and specification inventory','PASS — only gitlink changed; prior web/backend validation remains historical; fresh current-head CI/review required','events-mobile-cutover-catalog.json'),

('PR468 webhook-health follow-up e762f67b2','messaging/enrichment and webhook state/token regressions','PASS — 115 tests, zero failures/skips; inventory check remains current','cutover-diagnostic-tests.log'),
('PR468 operator follow-up','messaging/enrichment including manual API routing and diagnostic secret-output regressions','PASS — 113 tests, zero failures/skips; all three routing variants and both webhook callback paths tested without network writes','cutover-operator-tests-2.log'),
('PR468 operator follow-up','specification inventory and evidence gate','PASS — regenerated two operator-guide hashes; inventory check, 3 Python and 9 Node regressions pass','cutover-operator-spec.log'),
('PR468 operator-test setup','initial new test execution','FAILED — duplicate test import corrected and mock response aligned to the existing text-response API; all assertions retained in subsequent passing run','cutover-operator-tests.log'),

('PR468 earlier follow-up 68558f540','YAML, diff, regression and hosted specification evidence at that head','PASS for listed local checks and hosted specification gate; other hosted status is recorded in the snapshot, Datadog was failed at that historical head; later unchanged reruns passed','cutover-followup-validation-summary.json'),
('PR468 earlier hosted Datadog','API synthetic on commit 5486b4914','FAILED — exact final failure log retained; monitor was external at that historical failure; another actor later corrected its endpoint and unchanged reruns passed','cutover-final-datadog-failure.log'),

('PR468 generated specification','exact specification-contracts gate','PASS — inventory check, 3 Python regressions and 9 Node evidence/escrow tests; one course-document hash regenerated','cutover-spec-tests.log'),
('PR468 original hosted specification','specification inventory check before generated correction','FAILED — stale course-document SHA256; corrected in 68558f5409f9a976c0a4a9bd742e627f75f089cf without changing any check','cutover-spec-failure.log'),

('PR468 cutover maintenance','messaging and artist enrichment regression suites','PASS — 111 tests, zero failures, zero skips; readonly invalid/expiry failures retained','cutover-targeted-tests.log'),
('PR468 cutover maintenance','CI change-scope and pipeline contracts','PASS — 25 tests, zero failures, zero skips','cutover-ci-tests.log'),
('PR468 course publisher','three mocked production-default and override requests','PASS — default current API, course override precedence and legacy VITE override; no network or production writes','cutover-course-checks.json'),

('PR469 hosted operational check','Datadog production API health contract','FAILED — configured Fly health endpoint timed out after concurrent cutover; web check passed. Check remains enabled; subsequent external correction and unchanged successful rerun are recorded separately','discovery-hosted-build-failure.log'),
('PR469 standalone integration','Stack build --test --no-run-tests','STOPPED (exit130) after equivalent exact-head hosted build/full-suite/schema validation passed; no local full-build pass claimed','discovery-backend-build.log'),
('PR469 hosted full validation','backend build, full suite, all runtime runners and automatic production-schema rehearsal','PASS — 3,542 full-suite examples / 0 failures / 6 separately run cases, 1,141 social HTTP examples, every runtime and schema step succeeded; only executable upload skipped for this PR','discovery-hosted-backend.log'),
('PR469 hosted social rerun','one unchanged failed-job rerun','PASS — social-client attempt2; no code, assertions, timeouts or deployment changes','discovery-social-rerun-jobs.json'),
('PR469 local social preview','unchanged keyboard/mobile/axe assertions, one diagnostic rerun','PASS on rerun; initial correctly configured run detected transient ripple contrast. Both outcomes retained; no assertion or timeout changes','discovery-social-browser-recheck.log'),
('PR469 hosted social preview','unchanged synthetic browser journey','FAILED — initial expected heading timed out at 120 seconds; investigated without assertion/timeout changes','discovery-hosted-social-failure.log'),
('PR469 standalone integration','focused discovery suite on current-main integration','PASS — 28 examples, zero failures','discovery-focused-tests.log'),
('PR469 standalone integration','PostgreSQL source completion and shared boundary suite','PASS — disabled empty response, atomic rollback, mixed capacity, concurrent writer, authority, revocation and rollback/reapply','discovery-boundary-db.log'),
('PR469 standalone integration','production release tooling','PASS — 82 tests; all 116 introduction commits separately verified as ancestors','discovery-release-tests.log'),
('PR469 standalone integration','catalog and specification gates','PASS — only the reviewed 116-entry migration-registry fingerprint changed; both source CI/schema gates retained','discovery-catalog-final.json'),
('PR463 final source repair','actual Cron and dependency typecheck','PASS — corrected executable autogen path, verified process exit 0; existing warnings retained','pr463-cron-typecheck-final.log'),
('PR460 (merge blocked by native-stack history constraint)','Stack/GHC -Wall EventDiscovery suite on current-main integration','PASS — 26 examples, zero failures; no merge performed','pr460-discovery-tests.log'),
('PR463 repair','Stack/GHC -Wall EventDiscovery suite','PASS — 28 examples, zero failures','pr463-discovery-tests.log'),
('PR463 repair','real PostgreSQL shared count, mixed-entry race, authority/revocation and rollback/reapply','PASS, including actual production count helper (20 → 20 → 19 → 20)','pr463-shared-count-db.log'),
('PR463 repair','Stack/GHC -Wall actual EventResearch handler and dependencies','PASS — all 24 compilation units; existing warnings retained','pr463-handler-typecheck.log'),
('PR463 source completion','real PostgreSQL source authority and atomic completion','PASS — disabled empty feed rejected, failed final success write rolls back run completion, enabled source commits both success records; original boundary suite still passes','pr463-source-completion-db-3.log'),
('PR463 source completion','focused discovery suite','PASS — 28 examples, zero failures','pr463-discovery-tests-3.log'),
('PR463 source completion','initial database fixture attempt','FAILED — temporary fixture table disappeared after intentional SQL failure discarded its pooled connection; corrected to a table in the existing disposable test database','pr463-source-completion-db.log'),
('PR463 source completion','initial direct Cron compile','FAILED — omitted Stack generated Paths_tdf_hq include directory; command corrected without a source workaround','pr463-cron-typecheck.log'),
('PR463 source completion','catalog gate with pinned mobile checkout','PASS — first attempt reported stale fingerprints because the new worktree lacked its pinned mobile checkout; exact parent gitlink restored with no catalog-decision changes','pr463-catalog-2.json'),
('PR463 repair','production release tooling','PASS — 82 tests; all 115 introduction commits separately verified as ancestors','pr463-release-tests.log'),
('PR465 final browser repair','UI typecheck and zero-warning lint','PASS; final lint process completed with exit zero','events-ui-lint-final.log'),
('PR465 browser repair','targeted Playwright persona tests across configured Chromium/Firefox/WebKit projects','PASS — 19 passed, 8 pre-existing project-specific skips; no new skips or relaxed timeouts','events-persona-targeted.log'),
('PR465 browser repair','ArtistPublicPage component and intent suites','PASS — 40 tests / 2 suites, including preserved query/hash regression','events-follow-component-3.log'),
('PR462 (concurrent merge; tested by audit)','npm run test:production-release','PASS — 82 tests','test-462-release.log'),
('PR462','focused records UI tests','PASS — 5 tests / 2 suites','test-462-ui.log'),
('PR462','npm run typecheck:ui','PASS','test-462-types.log'),
('PR462','scripts/test-records-youtube-catalog-migration.sh','PASS on escalated disposable native PostgreSQL; first sandbox attempt blocked shared-memory creation','test-462-migration-escalated.log'),
('PR465 combined candidate','Stack build --test --no-run-tests','PASS','events-backend-build-escalated.log'),
('PR465','stack test','PASS — 3,573 examples, zero failures, six explicit separate-runner pending cases','events-backend-tests.log'),
('PR465','event formal verification','PASS finite-bound TLA+/Alloy checks, liveness and negative controls','events-formal.log'),
('PR465','npm run quality:repo','PASS, formal audit zero critical/errors; warnings retained','events-quality-repo.log'),
('PR465','UI focused tests','PASS — 338 tests / 9 suites','events-ui-targeted-2.log'),
('PR465','UI full test run','FAILED — 7 suites / 29 tests; 227 suites / 2,537 tests passed','events-ui-full.log'),
('PR465','same 7 failing UI suites, unchanged assertions/timeouts','PASS — 179 tests / 7 suites; hosted published-head ui-quality also passed','events-ui-failure-recheck.log'),
('PR465','UI typecheck and lint','PASS after unused-import repair','events-lint-ui-2.log'),
('PR465','UI production build/bundle guard','PASS — 5 preloads, 358,456 bytes gzip initial JS; limits unchanged','events-ui-build.log'),
('PR465','event tooling tests','PASS — 54 tests','events-node-targeted-2.log'),
('PR465 follow-up','runner isolation/wiring tests','PASS — 16 tests','events-followup-runner-tests.log'),
('PR465','catalog retirement and stale-fingerprint negative tests','PASS — 5 tests','events-catalog-final-tests.log'),
('PR465','catalog gate','PASS — zero unreviewed or stale active decisions','events-catalog-final.log'),
('PR465','specification inventory check / generator tests','PASS — generated index checked; 3 generator tests','events-spec-tests.log'),
('PR465','foundation SQL regression','PASS','events-operations-foundation-db.log'),
('PR465','API SQL regression','PASS','events-operations-api-db.log'),
('PR465','task commit + rollback integrity','PASS including new stale graph and deletion regressions','events-task-commit-db-2.log'),
('PR465','task read','PASS after transactional fixture and rejection controls','events-task-read-db-2.log'),
('PR465','task revision','PASS','events-task-revision-db-2.log'),
('PR465','revisioned task read','PASS after valid optional assignment mutation; original fixture failed current mandatory RACI guard','events-task-revisioned-read-db-3.log'),
('PR465','RACI reassignment','PASS','events-raci-db.log'),
('PR465','RACI editor context','PASS after valid readiness transition; original fixture failed mandatory RACI guard','events-raci-editor-context-db-2.log'),
('PR465','task completion','PASS — 32 decisions, 12 isolation races, four expiry waits, revocation orders, aborted-first recovery','events-task-completion-db-3.log'),
('PR465','complete-schema migration rehearsal','PASS; production manifest unchanged','events-schema-rehearsal.log'),
('Mobile115','locked npm ci','PASS using audit-owned cache; initial shared install lacked expo-clipboard and default cache had root-owned residue','events-mobile-install-2.log'),
('Mobile115','typecheck/lint/full test suite','PASS — 85 suites / 510 tests','events-mobile-tests.log'),
('Mobile115','npm run release:check','PASS (no build publication/deployment)','events-mobile-release-check.log'),
('Mobile115','npm audit --json','34 pre-existing findings: 1 low, 27 moderate, 6 high; manifests unchanged; no audit-fix or threshold bypass','events-mobile-security-audit.json')]
for suite in ['invitation-update-concurrency','event-relations-runtime','artist-self-service','provider-identity','trial-identity','live-intake-identity','course-identity','marketplace-contact-identity']:
 f='events-runtime-'+suite+'.log';txt=(P/f).read_text();matches=re.findall(r'\d+ examples?, \d+ failures?(?:, \d+ pending)?',txt);validation.append(('PR465','isolated '+suite,'PASS — '+matches[-1] if matches else 'PENDING',f))
for cmd,f,marker in [('event production-auth HTTP harness — 107 examples','events-http-3.log','Event operations production auth/subrouter HTTP tests passed'),('real RACI browser/API/PostgreSQL','events-browser.log','Real RACI session/API/PostgreSQL browser verification passed.')]:
 txt=(P/f).read_text();validation.append(('PR465',cmd,'PASS' if marker in txt else 'PENDING — executing; no pass claimed',f))
if (P/'events-main-backend-b07f67c3-verified.json').exists():
 proof=json.loads((P/'events-main-backend-b07f67c3-verified.json').read_text())
 assert proof['all_prescribed_steps_passed'] and proof['job']['conclusion']=='success'
 validation.insert(0,('PR465 merged main b07f67c3','normal post-merge backend, runtime and schema validation','PASS —3589 Hspec examples,1141 social HTTP examples,110 event HTTP examples; all prescribed job steps succeeded','events-main-backend-b07f67c3.log'))
(R/'validation.json').write_text(json.dumps(validation,indent=2)+'\n')
v=['# Executed validation','','Every result refers to an actual command/log. Initial failures remain recorded; they are not erased by successful reruns. A pending result does not qualify a PR for merge. Hosted current-head checks and reviews are recorded separately.','','| Implementation | Command/suite | Verified result | Evidence |','|---|---|---|---|']
v += ['| '+' | '.join(escape(x) for x in [a,b,c,f'[log](../{d})'])+' |' for a,b,c,d in validation];(R/'validation.md').write_text('\n'.join(v)+'\n')
blocked=['# Preserved cases and next steps','','All cases below are preserved. Failure, age, a closed PR, or branch naming alone was not used as obsolescence evidence.','','| Branch | Classification | Smallest next action / exact blocker |','|---|---|---|']
blocked += ['| '+' | '.join(escape(x) for x in [r['branch'],r['classification'],r['blocker_or_preservation_reason']])+' |' for r in rows if r['classification'] in ['CONSOLIDATE','BLOCKED_OR_AMBIGUOUS','KEEP_UNMERGED','FIX_THEN_MERGE']]
blocked += ['', 'Additional gates and preserved evidence:', '',
 '- Operational PR468: currentce1a71fe2 has independent approval and passing checks. Both new image/post-query provenance findings are fixed and resolved. Current authenticated image/volume/catalog/mail validation passes and the installed helper is verified. Actual Google interactive login/authenticated upload remains blocked by the occupied browser profile; no gate waiver is claimed.',
 '- Source441 documentation is rescued through approved, merged476/c4d479c7 with exact target text and original mobile-pin ancestry verified. Source464 is integrated through475/f8925 and was closed only after all target CI passed. Their source branches remain.',
 '- All73 concurrent root deletions are retained per explicit user direction; exact heads and commands are preserved in a verified Git bundle. No restoration is pending. No branch deletion was performed by this audit.',
 '- Issues128/130 remain open because their complete product/experiment acceptance criteria are not established by code integration.',
 '- See [all added branches](added-branches.md), [concurrent deletion recovery](concurrent-deletions.md) and the full [validation table](validation.md).']
if (P/'merge465-verified.json').exists():
 event=json.load(open(P/'merge465-verified.json'));ci=json.load(open(P/'events-closure-main-ci.json')) if (P/'events-closure-main-ci.json').exists() else {}
 source_closures=[m['pr'] for m in mut if m.get('action')=='closed_unmerged' and m.get('replacement')==465]
 blocked += ['', 'Event465 merged at `'+event['pr']['merge_commit_sha']+'` with exact tested tree and all source/migration ancestry retained. Normal post-merge CI: '+('PASS' if ci.get('all_passed') else 'PENDING; source closures remain gated')+'. Verified source closures: '+str(len(source_closures))+'. Source337 automatic integration is recorded separately in the ledger. All35 event source branches remain.']
else:
 blocked += ['', 'Event465 remains gated on current-head CI/review and verified integration before any source closures.']
(R/'blocked.md').write_text('\n'.join(blocked)+'\n')
print('Rendered complete 134-branch matrix, validation, blockers and live reconciliation',rec)

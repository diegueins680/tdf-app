# PR 14 — authenticated, typed task/RACI read API

Base: `feat/event-task-read-projection`, draft PR 354, commit
`a1b92c220691b694e5f53e7deaf4d455ce46bc44`.
Branch: `feat/event-task-read-api`. Draft only; no merge, deployment or provider activation.

## Scope and formal refinement

[TA-01–05](task-read-api-contract.md) refine the existing TR-01–08 SQL projection into
`GET /event-operations/events/{eventId}/tasks/{activityId}`. No alternate event or task
domain is introduced. The route derives the actor from production authentication and
uses `runEventOperationsSessionDb` / `withCurrentAuthSession` in the same transaction
as `loadTask`. Existing lifecycle snapshot/transition routes remain compatible.

The pinned TLC/Alloy suite completed successfully before adapter implementation. The
unchanged `TaskRead`, `TaskReadStructure`, `SessionFence` and `SnapshotRead` models apply;
all documented finite scopes and fairness limitations remain. The new JSON/numeric/header
contracts are checked executably, not presented as universal mathematical proofs.

The first real HTTP contract test reproduced the missing route: expected 200, actual 404;
the other 36 existing examples passed. The adapter was then implemented and that regression
passed. No existing test, exception guard or CI gate was disabled to obtain this result.

## Implementation and privacy

- Servant route returns the minimal canonical projection with `Cache-Control: private, no-store`.
  It exposes no actor/time query argument, title, notes, dates, contacts, dependencies or history.
- Strict Aeson decoding checks nested allowlists, canonical status/RACI enums, target identity,
  positive safe integers, policy version, unique party-role pairs and consistent attention.
  Corrupt SQL responses fail with sanitized 503; NULL maps to opaque 404 not_found.
- JSON-number captures, IDs and versions are bounded to 1–9007199254740991 for this new API.
  Larger values fail closed instead of silently rounding in JavaScript. This is an explicit
  initial transport limit, not a modification of existing BIGINT data or other API contracts.
- Web client uses generated OpenAPI types plus strict runtime validation and no-store requests.
  Malformed successful responses produce a fixed Spanish error, never raw decoder contents.
  There is no new cache, offline queue, write command, aggregate ETag or placeholder UI.
- Local and hosted HTTP runners apply the existing task-commit and task-read migrations before
  the opted-in task fixture. Safety guards and disposable-only connection rules are preserved.

## Executed verification

From the repository root:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar bash scripts/verify-event-operations-formal.sh
sh scripts/test-event-operations-http.sh
npm run generate:api:ui
npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/api/eventOperations.test.ts
npm run test:event-operations-http-runners
npm run quality:repo
npm run typecheck:ui
```

- Formal: PASS, exit 0; all positive TLC models, 20 expected negative controls, two SAT
  Alloy scenarios and 11 UNSAT assertions within [documented bounds](../../formal/event-operations/README.md).
- HTTP/Stack focused harness: initial implemented run PASS, 62 examples, zero failures,
  including two 100-case property checks. It compiles the actual authentication, Servant
  subrouter and database adapter through Stack in its separate object cache, then serves
  real HTTP against disposable PostgreSQL 16. No mocked server/database success.
- Web client: initial run 28/28 PASS. These are client contract tests with a mocked transport,
  not browser E2E evidence; the independent HTTP suite supplies real-server coverage.
- Final reruns after adding explicit numeric-boundary examples: HTTP/Hspec **63/63 PASS**,
  web **29/29 PASS**. Re-running web type generation produced the identical SHA-256
  `b5221a54fad87666332abc4b2d6d3d0ba4863befb42faf0a1bf7e2d1a2082351`.
- HTTP runner guards/dependency ordering: 4/4 PASS. Focused ESLint: PASS. OpenAPI parses;
  generation adds only the new task definitions/operation to the generated web file.
  Shell syntax, whitespace and all 86 local documentation links pass.
- Repository quality: PASS. The heuristic audit reports 0 critical/0 errors and 355 warnings.
  Its new resource warning matches the word “with” in a pure QuickCheck example title, not
  a resource acquisition. The heuristic was not weakened or confused with TLC/Alloy.

### Full web typecheck is blocked by existing onboarding imports

`npm run typecheck:ui` completed with exit 2:

```text
src/pages/LoginPage.tsx(56,10): TS2305: no exported member markWebSignupCompleted
src/routes/AppShell.tsx(23,10): TS2305: no exported member retryPendingFirstValueCompletion
```

Those two files and `src/analytics/onboardingProgress.ts` are byte-identical to the base
commit (verified with `git diff <base> -- <three paths>`). `git show <base>:<path>` confirms
the same imports while the module exports only `captureReconciledFirstValue` and
`captureFirstValueOnce`. This is source evidence of a pre-existing blocker; a full baseline
typecheck in a second worktree was not run. No onboarding code or global gate was changed.

A separate TypeScript program rooted at `eventOperations.ts`, its test and `vite-env.d.ts`
passed with the **unchanged** parsed `tdf-hq-ui/tsconfig.json` compiler options, including
their transitive client/generated-type dependencies. This focused diagnostic does not
replace the failed whole-web gate or establish application-wide compatibility.

The focused diagnostic uses TypeScript directly, without writing or weakening a project config:

```sh
node --input-type=module <<'NODE'
import ts from 'typescript';
import path from 'node:path';
const root = path.resolve('tdf-hq-ui');
const config = ts.readConfigFile(path.join(root, 'tsconfig.json'), ts.sys.readFile);
if (config.error) throw new Error(ts.flattenDiagnosticMessageText(config.error.messageText, '\n'));
const parsed = ts.parseJsonConfigFileContent(config.config, ts.sys, root);
const roots = ['src/api/eventOperations.ts', 'src/api/eventOperations.test.ts', 'src/vite-env.d.ts']
  .map(file => path.join(root, file));
const diagnostics = [...parsed.errors, ...ts.getPreEmitDiagnostics(ts.createProgram(roots, parsed.options))];
if (diagnostics.length) {
  console.error(ts.formatDiagnosticsWithColorAndContext(diagnostics, {
    getCanonicalFileName: name => name, getCurrentDirectory: ts.sys.getCurrentDirectory,
    getNewLine: () => '\n',
  }));
  process.exit(1);
}
console.log('Focused task client/types PASS; full UI gate remains separate.');
NODE
```

## Migration, rollback and remaining limitations

No production SQL, migration registration, schema, permissions, provider flags or submodule
pointer changes. The task migrations remain excluded from the production manifest. Roll back
the route/client consumer first; retain the harmless SQL functions or use their function-only
rollback after all consumers stop. No domain rows or immutable history need to be deleted.

The inherited long-running `stack test tdf-hq --fast --no-run-tests --no-terminal` build
started before this adapter and compiled its earlier EventOperations modules before these
edits. It subsequently detected source changes and began recompiling the server modules;
it is still running at this checkpoint. Do not treat it as a completed, stable-head full
build/test. The separate focused harness verifies the changed modules; full current-head
compilation/test remains required.

Mobile submodule `53569fc4baa842a6882235d9a12c4ee68c44ff24` is uninitialized in this worktree.
No mobile generation, compilation, screenshots, accessibility/browser E2E, performance or
provider tests are claimed. Hosted workflow configuration does not mean remote CI passed.
Current-token fencing does not add permanent credential revocation or in-flight global-role
catalog revalidation. RACI attention reports cardinality/time gaps, not membership validity.

Next: repair the owning onboarding integration before a green whole-web gate; then finish
field-visible task details/workspace views, contextual membership/removal, versioned commands,
history, aggregate concurrency tokens and subsequent event-operation phases. This PR is not
end-to-end product completion or production readiness.

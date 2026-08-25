# Mobile Release Smoke Checklist

Use this for one exact build on one exact device/session.
Do not write "looks good" or "ready" without filling pass/fail fields.
Every `FAIL` must name one blocker owner.

## Smoke-runnable handoff gate
Complete this block before any login/onboarding/navigation testing. If this gate fails, stop the run and record only the blocker-driven verdict.

- Handoff source (report entry / timestamp):
- Simulator/device target:
- App path / artifact used:
- Exact install command:
- Exact launch command:
- Launch contract type: `PACKAGER-BACKED` / `EMBEDDED-JS-BUNDLE`
- If `PACKAGER-BACKED`: exact Metro/dev-server start command, URL/port, and same-run proof it was running:
- If `EMBEDDED-JS-BUNDLE`: exact proof the app can launch without Metro / packager:
- First screen after launch: `APP UI` / `RUNTIME ERROR` / `OTHER`
- If first screen is not `APP UI`, stop here and record:
  - Exact visible error text:
  - Blocker owner:
  - Required next handoff piece:

Rules:
- Do not start the six release-critical flows unless the first screen reaches real app UI beyond any runtime error screen.
- `No script URL provided`, `unsanitizedScriptURLString = (null)`, or any equivalent Metro/bundle failure means smoke did not start.
- Do not infer missing install, launch, Metro, URL, or artifact details from older runs; every handoff field above must come from the same run that produced the artifact being tested.

### Copy/paste handoff block
Platform should hand Release this exact filled block in the same report entry that claims the build is smoke-runnable:

```md
### iOS smoke-runnable handoff
- Handoff timestamp:
- Simulator target:
- Artifact path:
- Exact install command:
- Launch contract type: `PACKAGER-BACKED` / `EMBEDDED-JS-BUNDLE`
- If `PACKAGER-BACKED`: exact Metro/dev-server start command:
- If `PACKAGER-BACKED`: URL/port and same-run proof it is reachable:
- If `EMBEDDED-JS-BUNDLE`: exact proof the app launches without Metro:
- Exact launch command:
- First-screen expectation before Release starts flow smoke: `APP UI`
```

## Test metadata
- Date/time:
- Tester:
- Build lane/profile:
- Build ID / artifact URL:
- App version / runtime version:
- Commit SHA:
- Device + OS:
- Network used (wifi/cellular):
- Evidence bundle (screenshots/video/log path):

## Release verdict
- RC verdict: `GO` / `NO-GO`
- Shipping decision: `SHIPPABLE` / `NOT YET SHIPPABLE` / `ONE MORE BOUNDED FIX/BUILD CYCLE`
- Summary in 2-4 lines:
- Exact ship blocker (required if not shippable):
- Blocker owner (required if not shippable):
- External review lane status: `BACKGROUND ONLY` / `ACTIVE BLOCKER`
- Exact next shipping step:

## Release-critical flows

### 1) Username/password login
- Result: `PASS` / `FAIL`
- Steps run:
- Exact observed outcome:
- If fail, blocker owner:

### 2) Google login
- Result: `PASS` / `FAIL`
- Steps run:
- Exact observed outcome:
- If fail, blocker owner:

### 3) Onboarding
- Result: `PASS` / `FAIL` / `NOT SHOWN`
- Entry point used:
- Exact observed outcome:
- If fail, blocker owner:

### 4) Primary navigation
- Result: `PASS` / `FAIL`
- Screens/tabs visited:
- Exact observed outcome:
- If fail, blocker owner:

### 5) Core task flow
- Exact flow exercised:
- Result: `PASS` / `FAIL`
- Start state:
- End state:
- Exact observed outcome:
- If fail, blocker owner:

### 6) Top error states
Record only the states actually exercised in this session.

#### Invalid credentials / rejected auth
- Result: `PASS` / `FAIL` / `NOT TESTED`
- Exact observed outcome:
- If fail, blocker owner:

#### Google auth cancel/failure handling
- Result: `PASS` / `FAIL` / `NOT TESTED`
- Exact observed outcome:
- If fail, blocker owner:

#### Network interruption / offline handling
- Result: `PASS` / `FAIL` / `NOT TESTED`
- Exact observed outcome:
- If fail, blocker owner:

#### Permission denial on a flow touched in this run
- Permission(s): camera / photos / location / other
- Result: `PASS` / `FAIL` / `NOT TESTED`
- Exact observed outcome:
- If fail, blocker owner:

## Release blockers from this run
- Blocker 1:
- Owner:
- Ship impact:
- Next action:

- Blocker 2:
- Owner:
- Ship impact:
- Next action:

## Submission notes produced by this run
- Any store-copy implications:
- Any screenshot updates needed:
- Any reviewer/demo-access updates needed:

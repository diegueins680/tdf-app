# Mobile Release Smoke Checklist

Use this for one exact build on one exact device/session.
Do not write "looks good" or "ready" without filling pass/fail fields.
Every `FAIL` must name one blocker owner.

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
- Summary in 2-4 lines:
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

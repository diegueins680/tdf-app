# Radio validity continuation, 2026-11-01

Base: PR670 head73a94de9c609e4c4b37b0abe552d0fd78725553a. Independent local follow-up only; do not duplicate or mutate PR655 (head1bfad302 observed).

Provider already implements bounded rendition retries and terminal next-track recovery. Radio main list already excluded runtime unavailable IDs. This patch applies a shared available-station projection to visible catalogue, hidden catalogue and recent shortcuts, excluding empty URLs too. No persistent health verdict or proactive decoding probe is added.

Focused Jest: four suites,72tests PASS. Changed-source ESLint PASS. Initial Jest setup failed before tests due uninitialized unrs-resolver after ignore-scripts install; npm rebuild fixed setup. Initial expanded-catalogue regression failed rendering with invalid element type; replaced with helper controls and existing provider recovery tests. No native-browser or physical-device acceptance is claimed. Typecheck tracked separately; no production mutation, PR write, push, merge or deployment.

Remaining: integrate with existing670 owner, persistent source-versioned health/probes and owner repair, exact-head hosted/browser checks, independent review, protected dependency integration and release admission. Keep670 draft and auto-merge off. Do not equate local filtering with universal availability.

This conversation already runs in its own Terminal ttys011 (observed PID67699); minimization runs separately onttys013. Do not spawn duplicate conversations or touch their tabs/files.

## Published follow-up

Initial availability commit76a9ee6c9 pushed to fix/radio-unavailable-surfaces-20261101 with explicit user authorization. PR655 is now merged (observed2026-11-01T22:32:33Z), no mutations from this conversation. PR670 head remains73a94de9 draft.

Continued recovery inspection reproduced two actual failing assertions on the pushed provider: waiting at10seconds postponed the15second deadline; timeupdate without a changed position canceled it. Both retained stalled rather than selecting good. Fixed watchdog clearing to reset timer ownership, ignored repeated waiting while a deadline exists, and accepted only source-matched position advancement from timeupdate. Controls preserve actual progress, rearm a later stall and preserve explicit pause. Final76tests/four suitesPASS, changed-source ESLintPASS, UI TypeScriptPASS. No browser/physical-device qualification.

Pinned Mobile7586be92 initialized solely in isolated worktree for canonical metadata generation. Source admission does not establish deployment acceptance or persistent media health. No new PR, merge, production mutation or deployment.

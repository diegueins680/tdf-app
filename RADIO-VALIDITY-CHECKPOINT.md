# Radio validity continuation, 2026-11-01

Base: PR670 head73a94de9c609e4c4b37b0abe552d0fd78725553a. Independent local follow-up only; do not duplicate or mutate PR655 (head1bfad302 observed).

Provider already implements bounded rendition retries and terminal next-track recovery. Radio main list already excluded runtime unavailable IDs. This patch applies a shared available-station projection to visible catalogue, hidden catalogue and recent shortcuts, excluding empty URLs too. No persistent health verdict or proactive decoding probe is added.

Focused Jest: four suites,72tests PASS. Changed-source ESLint PASS. Initial Jest setup failed before tests due uninitialized unrs-resolver after ignore-scripts install; npm rebuild fixed setup. Initial expanded-catalogue regression failed rendering with invalid element type; replaced with helper controls and existing provider recovery tests. No native-browser or physical-device acceptance is claimed. Typecheck tracked separately; no production mutation, PR write, push, merge or deployment.

Remaining: integrate with existing670 owner, persistent source-versioned health/probes and owner repair, exact-head hosted/browser checks, independent review, protected dependency integration and release admission. Keep670 draft and auto-merge off. Do not equate local filtering with universal availability.

This conversation already runs in its own Terminal ttys011 (observed PID67699); minimization runs separately onttys013. Do not spawn duplicate conversations or touch their tabs/files.

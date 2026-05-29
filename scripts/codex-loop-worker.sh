#!/usr/bin/env bash
set -euo pipefail

MODE="${1:?usage: codex-loop-worker.sh <implement|logical-fix|formal-fix|ux-fix|ui-fix|ci-repair> <task-file>}"
TASK_FILE="${2:?usage: codex-loop-worker.sh <implement|logical-fix|formal-fix|ux-fix|ui-fix|ci-repair> <task-file>}"
REPO_ROOT="${CONTINUOUS_LOOP_REPO_ROOT:-$(pwd)}"
ITERATION="${CONTINUOUS_LOOP_ITERATION:-unknown}"

if [ ! -f "$TASK_FILE" ]; then
  echo "Task file not found: $TASK_FILE" >&2
  exit 1
fi

PROMPT_FILE="$(mktemp "${TMPDIR:-/tmp}/codex-loop-prompt.XXXXXX")"
OUTPUT_FILE="$(mktemp "${TMPDIR:-/tmp}/codex-loop-output.XXXXXX")"
trap 'rm -f "$PROMPT_FILE" "$OUTPUT_FILE"' EXIT

case "$MODE" in
  implement)
    TASK_LABEL="implementation"
    TASK_GOAL="Read the improvement idea and implement it with the smallest high-signal change that adds user value."
    ;;
  logical-fix)
    TASK_LABEL="logical correctness remediation"
    TASK_GOAL="Read the logical correctness audit report and fix every listed issue in the codebase. Prefer explicit, predictable logic. Add or update tests that would have caught the flaw."
    ;;
  formal-fix)
    TASK_LABEL="formal methods remediation"
    TASK_GOAL="Read the formal methods audit report and fix every listed issue using explicit invariants, preconditions, postconditions, type contracts, and tests where appropriate."
    ;;
  ux-fix)
    TASK_LABEL="UX quality remediation"
    TASK_GOAL="Read the UX quality audit report and fix every listed issue. Make the UI simpler, more minimal, more intuitive, and more engaging. Reduce clutter and make affordances obvious."
    ;;
  ui-fix)
    TASK_LABEL="UI remediation"
    TASK_GOAL="Read the UI audit report and fix every listed issue in the report."
    ;;
  ci-repair)
    TASK_LABEL="CI remediation"
    TASK_GOAL="Read the CI report and make the smallest change that is most likely to restore green checks."
    ;;
  *)
    echo "Unsupported mode: $MODE" >&2
    exit 1
    ;;
esac

cat > "$PROMPT_FILE" <<EOF
You are running one unattended continuous-improvement-loop subtask in the repository at:
$REPO_ROOT

Iteration: $ITERATION
Subtask: $TASK_LABEL

$TASK_GOAL

Constraints:
- Stay scoped to this subtask only.
- You may inspect, edit, and test the repository locally.
- Build and test the Haskell backend in tdf-hq/ with stack ONLY. The project uses lts-24.42 (GHC 9.10.3) via tdf-hq/stack.yaml; use 'stack build' and 'stack test' (run from inside tdf-hq/). Do NOT use cabal or the system GHC — that is a different toolchain the project does not use, its build artifacts (dist-newstyle/) are ignored, and a green cabal build does not mean a green project build.
- Do not commit, push, pull, rebase, or start a long-running server.
- Do not modify or stage unrelated user artifacts, especially pre-existing files under screencast/meta-app-review/output/ or screencast/meta-app-review/frame-check/.
- Prefer the smallest defensible change over large speculative rewrites.
- If you touch logic, add or update verification when practical.
- If the task file is already satisfied, make no changes and explain why.
- End your final response with exactly one of:
  RESULT: done
  RESULT: blocked

Task file path: $TASK_FILE

Task file contents:
---
$(cat "$TASK_FILE")
---
EOF

CODEX_STDERR="$(mktemp "${TMPDIR:-/tmp}/codex-loop-stderr.XXXXXX")"
trap 'rm -f "$PROMPT_FILE" "$OUTPUT_FILE" "$CODEX_STDERR"' EXIT

set +e
cat "$PROMPT_FILE" | codex -a never -s workspace-write exec -C "$REPO_ROOT" --color never -o "$OUTPUT_FILE" - 2>"$CODEX_STDERR"
CODEX_EXIT=$?
set -e

cat "$OUTPUT_FILE"
cat "$CODEX_STDERR" >&2

if [ "$CODEX_EXIT" -ne 0 ]; then
  if grep -qi 'usage limit' "$OUTPUT_FILE" 2>/dev/null || grep -qi 'usage limit' "$CODEX_STDERR" 2>/dev/null; then
    echo "RESULT: blocked" >> "$OUTPUT_FILE"
    exit 0
  fi
  exit "$CODEX_EXIT"
fi

RESULT_MARKER="$(grep -Eo 'RESULT: (done|blocked)' "$OUTPUT_FILE" | tail -n1 || true)"

if [ -z "$RESULT_MARKER" ]; then
  echo "Codex worker did not emit a valid RESULT marker." >&2
  exit 1
fi

if [ "$RESULT_MARKER" = "RESULT: blocked" ]; then
  exit 1
fi

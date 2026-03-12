#!/usr/bin/env bash
set -euo pipefail

MODE="${1:?usage: codex-loop-worker.sh <implement|ui-fix|formal-fix|ci-repair> <task-file>}"
TASK_FILE="${2:?usage: codex-loop-worker.sh <implement|ui-fix|formal-fix|ci-repair> <task-file>}"
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
  ui-fix)
    TASK_LABEL="UI remediation"
    TASK_GOAL="Read the UI audit report and fix every listed issue in the report."
    ;;
  formal-fix)
    TASK_LABEL="formal remediation"
    TASK_GOAL="Read the formal verification report and fix every listed issue using explicit invariants and tests where appropriate."
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

cat "$PROMPT_FILE" | codex -a never -s workspace-write exec -C "$REPO_ROOT" --color never -o "$OUTPUT_FILE" -
cat "$OUTPUT_FILE"

RESULT_MARKER="$(grep -Eo 'RESULT: (done|blocked)' "$OUTPUT_FILE" | tail -n1 || true)"

if [ -z "$RESULT_MARKER" ]; then
  echo "Codex worker did not emit a valid RESULT marker." >&2
  exit 1
fi

if [ "$RESULT_MARKER" = "RESULT: blocked" ]; then
  exit 1
fi

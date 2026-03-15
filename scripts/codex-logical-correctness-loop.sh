#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STATE_DIR="${ROOT_DIR}/.tmp/codex-logical-correctness-loop"
LOG_DIR="${STATE_DIR}/logs"
BASELINE_FILE="${STATE_DIR}/baseline-status.txt"
BASELINE_PATHS_FILE="${STATE_DIR}/baseline-paths.txt"
RUN_LOG="${STATE_DIR}/runner.log"
mkdir -p "${LOG_DIR}"

CODEX_MODEL="${CODEX_LOOP_MODEL:-gpt-5.4}"
POLL_SECONDS="${CODEX_LOOP_POLL_SECONDS:-45}"
CI_TIMEOUT_MINUTES="${CODEX_LOOP_CI_TIMEOUT_MINUTES:-60}"
NO_RUNS_GRACE_SECONDS="${CODEX_LOOP_NO_RUNS_GRACE_SECONDS:-180}"
SLEEP_BETWEEN_CYCLES="${CODEX_LOOP_CYCLE_SLEEP_SECONDS:-20}"
ALLOW_DIRTY="${CODEX_LOOP_ALLOW_DIRTY:-0}"
MAX_CYCLES="${CODEX_LOOP_MAX_CYCLES:-0}"

log() {
  printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*" | tee -a "${RUN_LOG}"
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || {
    echo "Missing required command: $1" >&2
    exit 1
  }
}

trim_file() {
  local file="$1"
  local lines="${2:-400}"
  if [[ -f "$file" ]]; then
    tail -n "$lines" "$file"
  fi
}

extract_paths_from_porcelain() {
  while IFS= read -r line; do
    local path_spec
    path_spec="${line:3}"
    if [[ -z "$path_spec" ]]; then
      continue
    fi
    if [[ "$path_spec" == *" -> "* ]]; then
      printf '%s\n' "${path_spec%% -> *}"
      printf '%s\n' "${path_spec#* -> }"
    else
      printf '%s\n' "$path_spec"
    fi
  done | awk 'NF && !seen[$0]++'
}

capture_baseline() {
  git -C "${ROOT_DIR}" status --porcelain=v1 > "${BASELINE_FILE}"
  extract_paths_from_porcelain < "${BASELINE_FILE}" > "${BASELINE_PATHS_FILE}"
}

baseline_paths_block() {
  if [[ ! -s "${BASELINE_PATHS_FILE}" ]]; then
    return 0
  fi
  sed 's/^/- /' "${BASELINE_PATHS_FILE}"
}

repo_status_block() {
  git -C "${ROOT_DIR}" status --short
}

ensure_baseline_policy() {
  capture_baseline
  if [[ "${ALLOW_DIRTY}" != "1" ]] && [[ -s "${BASELINE_FILE}" ]]; then
    echo "Repository is dirty. Re-run with CODEX_LOOP_ALLOW_DIRTY=1 to preserve current changes as protected baseline." >&2
    echo "Current status:" >&2
    repo_status_block >&2
    exit 1
  fi
}

has_non_baseline_changes() {
  local paths_file
  paths_file="$(mktemp)"
  list_non_baseline_paths > "$paths_file"
  if [[ -s "$paths_file" ]]; then
    rm -f "$paths_file"
    return 0
  fi
  rm -f "$paths_file"
  return 1
}

list_non_baseline_paths() {
  local current current_paths tmp
  current="$(mktemp)"
  current_paths="$(mktemp)"
  tmp="$(mktemp)"
  git -C "${ROOT_DIR}" status --porcelain=v1 > "$current"
  extract_paths_from_porcelain < "$current" > "$current_paths"
  if [[ -s "${BASELINE_PATHS_FILE}" ]]; then
    grep -Fvx -f "${BASELINE_PATHS_FILE}" "$current_paths" > "$tmp" || true
  else
    cp "$current_paths" "$tmp"
  fi
  sed '/^$/d' "$tmp"
  rm -f "$current" "$current_paths" "$tmp"
}

stage_non_baseline_changes() {
  local paths_file="$1"
  local -a paths
  local path=""
  if [[ ! -s "$paths_file" ]]; then
    return 1
  fi
  paths=()
  while IFS= read -r path || [[ -n "$path" ]]; do
    paths+=("$path")
  done < "$paths_file"
  if [[ "${#paths[@]}" -eq 0 ]]; then
    return 1
  fi
  git -C "${ROOT_DIR}" add -A -- "${paths[@]}"
}

manual_commit_and_push() {
  local cycle_label="$1"
  local reason="${2:-fallback}"
  local branch paths_file message
  local -a paths
  local path=""
  branch="$(current_branch)"
  if [[ -z "$branch" ]]; then
    log "Wrapper fallback could not determine current branch."
    return 1
  fi

  paths_file="$(mktemp)"
  list_non_baseline_paths > "$paths_file"
  if [[ ! -s "$paths_file" ]]; then
    log "Wrapper fallback found no non-baseline paths to commit."
    rm -f "$paths_file"
    return 1
  fi
  paths=()
  while IFS= read -r path || [[ -n "$path" ]]; do
    paths+=("$path")
  done < "$paths_file"
  if [[ "${#paths[@]}" -eq 0 ]]; then
    log "Wrapper fallback could not parse any non-baseline paths."
    rm -f "$paths_file"
    return 1
  fi

  log "Wrapper fallback staging non-baseline paths after Codex ${reason}: $(paste -sd ', ' "$paths_file")"
  stage_non_baseline_changes "$paths_file"
  rm -f "$paths_file"

  if git -C "${ROOT_DIR}" diff --cached --quiet -- "${paths[@]}"; then
    log "Wrapper fallback found nothing staged after git add."
    return 1
  fi

  message="chore: codex logical correctness loop iteration ${cycle_label}"
  git -C "${ROOT_DIR}" commit -m "$message" -- "${paths[@]}"
  git -C "${ROOT_DIR}" push -u origin "$branch"
}

current_branch() {
  git -C "${ROOT_DIR}" branch --show-current | tr -d '[:space:]'
}

codex_prompt() {
  local name="$1"
  local prompt_file="$2"
  local output_file="$3"
  log "Running Codex step: ${name}"
  codex exec --full-auto -m "${CODEX_MODEL}" -C "${ROOT_DIR}" --color never -o "${output_file}" - < "${prompt_file}"
  trim_file "${output_file}" 200 | tee -a "${RUN_LOG}" >/dev/null || true
}

make_prompt_file() {
  local file="$1"
  cat > "$file"
}

wait_for_ci_runs() {
  local sha="$1"
  local output_file="$2"
  local deadline=$(( $(date +%s) + CI_TIMEOUT_MINUTES * 60 ))
  local no_runs_deadline=$(( $(date +%s) + NO_RUNS_GRACE_SECONDS ))
  local branch
  : > "$output_file"
  branch="$(current_branch)"
  while true; do
    local runs_file
    runs_file="$(mktemp)"
    gh run list \
      --repo "$(gh repo view --json nameWithOwner -q .nameWithOwner)" \
      --branch "$branch" \
      --commit "$sha" \
      --json databaseId,displayTitle,workflowName,status,conclusion,url,headSha \
      > "$runs_file"

    local total pending failed success
    total="$(jq 'length' "$runs_file")"
    pending="$(jq '[.[] | select((.status // "") != "completed")] | length' "$runs_file")"
    failed="$(jq '[.[] | select((.status // "") == "completed" and ((.conclusion // "") != "success" and (.conclusion // "") != "neutral" and (.conclusion // "") != "skipped"))] | length' "$runs_file")"
    success="$(jq '[.[] | select((.status // "") == "completed" and (.conclusion // "") == "success")] | length' "$runs_file")"

    if [[ "$failed" -gt 0 ]]; then
      cp "$runs_file" "$output_file"
      rm -f "$runs_file"
      return 10
    fi

    if [[ "$total" -gt 0 && "$pending" -eq 0 ]]; then
      log "All detected GitHub Actions are green for ${sha} (${success}/${total} successful)."
      cp "$runs_file" "$output_file"
      rm -f "$runs_file"
      return 0
    fi

    if [[ "$total" -eq 0 && $(date +%s) -ge "$no_runs_deadline" ]]; then
      log "No GitHub Actions detected for ${sha} after ${NO_RUNS_GRACE_SECONDS}s. Assuming no workflows matched."
      cp "$runs_file" "$output_file"
      rm -f "$runs_file"
      return 0
    fi

    if [[ $(date +%s) -ge "$deadline" ]]; then
      cp "$runs_file" "$output_file"
      rm -f "$runs_file"
      return 11
    fi

    log "Waiting for GitHub Actions for ${sha} (total=${total}, pending=${pending}, success=${success})..."
    rm -f "$runs_file"
    sleep "$POLL_SECONDS"
  done
}

build_detect_prompt() {
  local prompt_file="$1"
  make_prompt_file "$prompt_file" <<EOF
You are Codex working inside this repository:
${ROOT_DIR}

Task:
Detect anything that could be made more logical and correct.

Instructions:
- Audit logic, correctness, edge cases, invariants, broken assumptions, bad state transitions, wrong defaults, incorrect parsing/typing, and likely CI/runtime correctness issues.
- Prefer concrete, actionable findings over vague suggestions.
- Do NOT modify files in this step.
- Return a prioritized markdown list with file paths and why each issue matters.
- If you genuinely find nothing worth fixing right now, output exactly: NO_ISSUES

Protected baseline paths from before this loop started (do not stage/commit these unless explicitly required by the new fix):
$(baseline_paths_block)
EOF
}

build_implement_prompt() {
  local prompt_file="$1"
  local findings_file="$2"
  make_prompt_file "$prompt_file" <<EOF
You are Codex working inside this repository:
${ROOT_DIR}

Task:
Implement fixes for all of the logic/correctness issues listed below.

Requirements:
- Make the smallest robust changes that fix the issues.
- Run relevant local verification where practical.
- Update tests/docs/changelog if warranted.
- Do NOT commit or push in this step.
- Do NOT modify or stage protected baseline paths unless absolutely necessary for the new fixes.
- At the end, print exactly one of:
  IMPLEMENT_RESULT: changed
  IMPLEMENT_RESULT: no_changes
  IMPLEMENT_RESULT: blocked

Protected baseline paths from before this loop started:
$(baseline_paths_block)

Findings to fix:
---
$(cat "$findings_file")
---
EOF
}

build_commit_push_prompt() {
  local prompt_file="$1"
  make_prompt_file "$prompt_file" <<EOF
You are Codex working inside this repository:
${ROOT_DIR}

Task:
Commit and push the fixes from this iteration.

Requirements:
- Review the current git diff/status.
- Commit only files relevant to the current iteration's fixes.
- Do NOT stage or commit protected baseline paths that predated this loop unless they were intentionally part of the new fixes.
- Push the current branch to origin.
- If there is nothing to commit for this iteration, output exactly: COMMIT_PUSH_RESULT: no_changes
- If you successfully commit and push, end with exactly: COMMIT_PUSH_RESULT: pushed
- If blocked, end with exactly: COMMIT_PUSH_RESULT: blocked

Protected baseline paths:
$(baseline_paths_block)
EOF
}

build_ci_fix_prompt() {
  local prompt_file="$1"
  local sha="$2"
  local logs_file="$3"
  make_prompt_file "$prompt_file" <<EOF
You are Codex working inside this repository:
${ROOT_DIR}

Task:
GitHub Actions failed for commit ${sha}. Fix all issues evidenced by these CI logs.

Requirements:
- Focus only on issues supported by the logs.
- Make the smallest robust fixes.
- Run relevant local verification where practical.
- Do NOT commit or push in this step.
- Do NOT modify or stage protected baseline paths unless absolutely necessary for the CI fix.
- End with exactly one of:
  CI_FIX_RESULT: changed
  CI_FIX_RESULT: no_changes
  CI_FIX_RESULT: blocked

Protected baseline paths:
$(baseline_paths_block)

CI logs:
---
$(cat "$logs_file")
---
EOF
}

commit_push_result() {
  local output_file="$1"
  if grep -Fxq 'COMMIT_PUSH_RESULT: pushed' "$output_file"; then
    echo "pushed"
  elif grep -Fxq 'COMMIT_PUSH_RESULT: no_changes' "$output_file"; then
    echo "no_changes"
  elif grep -Fxq 'COMMIT_PUSH_RESULT: blocked' "$output_file"; then
    echo "blocked"
  else
    echo "unknown"
  fi
}

collect_failed_logs() {
  local runs_json_file="$1"
  local logs_file="$2"
  : > "$logs_file"
  jq -r '.[] | select((.status // "") == "completed" and ((.conclusion // "") != "success" and (.conclusion // "") != "neutral" and (.conclusion // "") != "skipped")) | [.databaseId, (.workflowName // .displayTitle // "unknown"), (.url // "")] | @tsv' "$runs_json_file" |
  while IFS=$'\t' read -r run_id run_name run_url; do
    {
      echo "===== FAILED RUN: ${run_name} (${run_id}) ====="
      echo "URL: ${run_url}"
      gh run view "$run_id" --log-failed 2>/dev/null || gh run view "$run_id" --log || true
      echo
    } >> "$logs_file"
  done
}

main() {
  require_cmd codex
  require_cmd gh
  require_cmd jq
  require_cmd git

  ensure_baseline_policy

  local cycle=1
  while [[ "$MAX_CYCLES" == "0" || "$cycle" -le "$MAX_CYCLES" ]]; do
    log "=== Cycle ${cycle} starting in ${ROOT_DIR} ==="

    local detect_prompt detect_output
    detect_prompt="$(mktemp)"
    detect_output="${LOG_DIR}/cycle-${cycle}-detect.md"
    build_detect_prompt "$detect_prompt"
    codex_prompt "detect" "$detect_prompt" "$detect_output"
    rm -f "$detect_prompt"

    if grep -Fxq 'NO_ISSUES' "$detect_output"; then
      log "Codex reported no logic/correctness issues. Sleeping ${SLEEP_BETWEEN_CYCLES}s."
      sleep "$SLEEP_BETWEEN_CYCLES"
      cycle=$((cycle + 1))
      continue
    fi

    local implement_prompt implement_output
    implement_prompt="$(mktemp)"
    implement_output="${LOG_DIR}/cycle-${cycle}-implement.md"
    build_implement_prompt "$implement_prompt" "$detect_output"
    codex_prompt "implement" "$implement_prompt" "$implement_output"
    rm -f "$implement_prompt"

    if grep -Fxq 'IMPLEMENT_RESULT: blocked' "$implement_output"; then
      log "Codex reported blocked during implementation. Stopping loop."
      exit 1
    fi

    if ! has_non_baseline_changes; then
      log "No non-baseline changes detected after implementation. Sleeping ${SLEEP_BETWEEN_CYCLES}s."
      sleep "$SLEEP_BETWEEN_CYCLES"
      cycle=$((cycle + 1))
      continue
    fi

    local commit_prompt commit_output
    commit_prompt="$(mktemp)"
    commit_output="${LOG_DIR}/cycle-${cycle}-commit-push.md"
    build_commit_push_prompt "$commit_prompt"
    local commit_codex_failed=0
    if ! codex_prompt "commit-push" "$commit_prompt" "$commit_output"; then
      commit_codex_failed=1
      log "Codex commit/push step exited non-zero for cycle ${cycle}; attempting wrapper fallback."
    fi
    rm -f "$commit_prompt"

    local commit_result
    commit_result="$(commit_push_result "$commit_output")"
    case "$commit_result" in
      pushed)
        ;;
      no_changes)
        log "Codex reported no changes to commit. Sleeping ${SLEEP_BETWEEN_CYCLES}s."
        sleep "$SLEEP_BETWEEN_CYCLES"
        cycle=$((cycle + 1))
        continue
        ;;
      blocked|unknown)
        if manual_commit_and_push "$cycle" "commit-push/${commit_result}"; then
          log "Wrapper fallback committed and pushed cycle ${cycle} after Codex result=${commit_result}."
        else
          if [[ "$commit_codex_failed" -eq 1 ]]; then
            log "Codex commit/push failed and wrapper fallback also failed. Stopping loop."
          else
            log "Codex reported ${commit_result} during commit/push and wrapper fallback failed. Stopping loop."
          fi
          exit 1
        fi
        ;;
    esac

    local sha
    sha="$(git -C "${ROOT_DIR}" rev-parse HEAD)"
    log "Pushed commit ${sha}. Polling GitHub Actions."

    while true; do
      local runs_json
      runs_json="${LOG_DIR}/cycle-${cycle}-runs.json"
      if wait_for_ci_runs "$sha" "$runs_json"; then
        break
      fi
      local rc=$?
      if [[ "$rc" -eq 11 ]]; then
        log "Timed out waiting for CI for ${sha}. Stopping loop."
        exit 1
      fi
      if [[ "$rc" -ne 10 ]]; then
        log "Unexpected CI polling result (${rc}) for ${sha}. Stopping loop."
        exit 1
      fi

      local ci_logs_file
      ci_logs_file="${LOG_DIR}/cycle-${cycle}-ci-failures.log"
      collect_failed_logs "$runs_json" "$ci_logs_file"

      local ci_fix_prompt ci_fix_output
      ci_fix_prompt="$(mktemp)"
      ci_fix_output="${LOG_DIR}/cycle-${cycle}-ci-fix.md"
      build_ci_fix_prompt "$ci_fix_prompt" "$sha" "$ci_logs_file"
      codex_prompt "ci-fix" "$ci_fix_prompt" "$ci_fix_output"
      rm -f "$ci_fix_prompt"

      if grep -Fxq 'CI_FIX_RESULT: blocked' "$ci_fix_output"; then
        log "Codex reported blocked during CI fix. Stopping loop."
        exit 1
      fi

      if ! has_non_baseline_changes; then
        log "Codex did not create non-baseline changes after CI failure. Stopping loop."
        exit 1
      fi

      commit_prompt="$(mktemp)"
      commit_output="${LOG_DIR}/cycle-${cycle}-commit-push-ci.md"
      build_commit_push_prompt "$commit_prompt"
      commit_codex_failed=0
      if ! codex_prompt "commit-push-after-ci-fix" "$commit_prompt" "$commit_output"; then
        commit_codex_failed=1
        log "Codex commit/push step after CI fix exited non-zero for cycle ${cycle}; attempting wrapper fallback."
      fi
      rm -f "$commit_prompt"

      commit_result="$(commit_push_result "$commit_output")"
      case "$commit_result" in
        pushed)
          ;;
        no_changes)
          log "No changes to commit after CI fix attempt. Stopping loop."
          exit 1
          ;;
        blocked|unknown)
          if manual_commit_and_push "${cycle}-ci" "commit-push-after-ci-fix/${commit_result}"; then
            log "Wrapper fallback committed and pushed CI follow-up for cycle ${cycle} after Codex result=${commit_result}."
          else
            if [[ "$commit_codex_failed" -eq 1 ]]; then
              log "Codex commit/push after CI fix failed and wrapper fallback also failed. Stopping loop."
            else
              log "Codex reported ${commit_result} after CI fix and wrapper fallback failed. Stopping loop."
            fi
            exit 1
          fi
          ;;
      esac

      sha="$(git -C "${ROOT_DIR}" rev-parse HEAD)"
      log "Pushed follow-up commit ${sha}. Re-polling GitHub Actions."
    done

    log "Cycle ${cycle} finished green. Sleeping ${SLEEP_BETWEEN_CYCLES}s before next cycle."
    sleep "$SLEEP_BETWEEN_CYCLES"
    cycle=$((cycle + 1))
  done
}

main "$@"

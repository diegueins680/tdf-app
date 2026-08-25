#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BACKEND_DIR="$ROOT/tdf-hq"

if [ ! -d "$BACKEND_DIR" ] || [ ! -f "$BACKEND_DIR/stack.yaml" ] || ! command -v stack >/dev/null 2>&1; then
  if [ "${REQUIRE_STACK:-0}" = "1" ]; then
    echo "✖ Missing stack or backend project. Install stack and ensure tdf-hq/stack.yaml exists." >&2
    exit 1
  fi
  echo "▶ Skipping Haskell checks: stack unavailable or backend project missing"
  exit 0
fi

STACK_ROOT_DIR="${STACK_ROOT:-$ROOT/.stack-root}"
mkdir -p "$STACK_ROOT_DIR"

echo "▶ Building and testing Haskell backend in one Stack invocation"
build_args=(--no-terminal test tdf-hq)
if [ -n "${BACKEND_BINARY_OUT:-}" ]; then
  binary_dir="$(dirname "$BACKEND_BINARY_OUT")"
  mkdir -p "$binary_dir"
  build_args+=(--copy-bins --local-bin-path "$binary_dir")
fi

(
  cd "$BACKEND_DIR"
  STACK_ROOT="$STACK_ROOT_DIR" stack "${build_args[@]}"
)

if [ -n "${BACKEND_BINARY_OUT:-}" ]; then
  copied_binary="$(dirname "$BACKEND_BINARY_OUT")/tdf-hq-exe"
  test -s "$copied_binary"
  if [ "$copied_binary" != "$BACKEND_BINARY_OUT" ]; then
    mv "$copied_binary" "$BACKEND_BINARY_OUT"
  fi
  chmod +x "$BACKEND_BINARY_OUT"
fi

#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
cd tdf-mobile

npx eas build --platform ios --profile preview --non-interactive

#!/usr/bin/env bash
set -euo pipefail

set -a
source config/default.env
set +a

stack setup
stack build
stack run

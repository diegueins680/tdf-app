#!/usr/bin/env bash
set -euo pipefail
echo "Applying PR patches for TDF-ui..."
git checkout -b feature/trials-ui || true
git am --3way *.patch
echo "Done. Now run: npm install && npm run dev"

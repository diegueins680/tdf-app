#!/bin/sh
# SYS-INVENTORY-001 / audit F-01: the committed Mobile gitlink must be reachable
# from the Mobile repository's main branch, so that contract checks validate a
# revision that main actually contains and the gitlink stays fetchable.
#
# Requires the tdf-mobile submodule to be initialized at the committed gitlink.
# Exit 0 when the gitlink is an ancestor of origin/main; exit 1 otherwise
# (the output names the missing commits); exit 2 on setup failure.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
gitlink=$(git -C "$root" ls-tree HEAD -- tdf-mobile | awk '$1=="160000"{print $3}')
test -n "$gitlink" || { echo "No committed Mobile gitlink" >&2; exit 2; }
checked_out=$(git -C "$root/tdf-mobile" rev-parse HEAD 2>/dev/null) || {
  echo "tdf-mobile submodule is not initialized" >&2; exit 2; }
test "$checked_out" = "$gitlink" || {
  echo "tdf-mobile checkout $checked_out differs from gitlink $gitlink" >&2; exit 2; }
fetch_main() { git -C "$root/tdf-mobile" fetch --quiet "$@" origin main; }
fetch_main || { echo "Could not fetch Mobile origin/main" >&2; exit 2; }
# CI clones the submodule with depth 1, and a fetch into a shallow repository
# can graft main as a parentless tip, which would wrongly report the gitlink as
# unreachable. Recover the commit history (blobless) before judging ancestry.
if ! git -C "$root/tdf-mobile" merge-base --is-ancestor "$gitlink" origin/main \
   && [ "$(git -C "$root/tdf-mobile" rev-parse --is-shallow-repository)" = true ]; then
  fetch_main --unshallow --filter=blob:none || fetch_main --unshallow || {
    echo "Could not recover Mobile history for the ancestry check" >&2; exit 2; }
fi
if git -C "$root/tdf-mobile" merge-base --is-ancestor "$gitlink" origin/main; then
  echo "Mobile gitlink $gitlink is reachable from Mobile main"
  exit 0
fi
echo "Mobile gitlink $gitlink is NOT reachable from Mobile main; commits missing from main:" >&2
git -C "$root/tdf-mobile" log --oneline origin/main.."$gitlink" >&2
exit 1

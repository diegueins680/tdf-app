#!/usr/bin/env bash
# Generate the production backup recovery key on the custodian's own Mac.
#
# Uses the age release pinned in ops/hetzner/recovery-tools.json and verifies
# its checksums before running. The private key is written to a mode-0600 file
# so the custodian can move it into their password manager; it is never
# printed, logged or sent anywhere. Only the public recipient is shown.
#
# Usage: scripts/generate-recovery-key.sh
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
pins="$repo_root/ops/hetzner/recovery-tools.json"

case "$(uname -s)-$(uname -m)" in
  Darwin-x86_64) platform=darwin-amd64 ;;
  Linux-x86_64) platform=linux-amd64 ;;
  *) echo "Unsupported platform $(uname -s)-$(uname -m); add pins to $pins first." >&2; exit 1 ;;
esac
version="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["version"])' "$pins")"

# Physical path: the installer refuses symlinked ancestors such as macOS /var.
work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT
chmod 700 "$work"
archive="$work/age.tar.gz"
url="https://github.com/FiloSottile/age/releases/download/${version}/age-${version}-${platform}.tar.gz"

echo "Downloading age ${version} for ${platform}..."
curl -fsSL --proto '=https' --tlsv1.2 -o "$archive" "$url"
# The reviewed installer checks the archive and both binaries against the pins.
python3 "$repo_root/scripts/install-recovery-tools.py" --archive "$archive" --destination "$work/age" >/dev/null \
  || { echo "Checksum verification failed; refusing to continue." >&2; exit 1; }
keygen="$work/age/age-keygen"

out="${HOME}/tdf-recovery-key-$(date +%Y%m%d).txt"
if [ -e "$out" ]; then echo "$out already exists; not overwriting." >&2; exit 1; fi

umask 077
"$keygen" -o "$out" 2>/dev/null
recipient="$(grep -m1 '^# public key: ' "$out" | sed 's/^# public key: //')"
recipient_hash="$(printf '%s' "$recipient" | shasum -a 256 | awk '{print $1}')"

cat <<EOF

Private key written to: $out   (mode 0600)

Next steps:
  1. Open that file and copy its ENTIRE contents into a new secure note in your
     password manager, titled "TDF production backup recovery key".
  2. Check the note saved correctly, then delete the file:
       rm -P "$out"
  3. Send Claude ONLY these two public lines (safe to share):

     recipient: $recipient
     recipient sha256: $recipient_hash

Never share the AGE-SECRET-KEY line with anyone, including Claude.
EOF

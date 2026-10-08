#!/bin/sh
set -eu

if [ "${DDEX_LICENSE_ACCEPTED:-}" != "yes" ]; then
  echo "Set DDEX_LICENSE_ACCEPTED=yes only after reviewing the DDEX evaluation/implementation licence." >&2
  exit 2
fi

output_dir=${1:-}
if [ -z "$output_dir" ]; then
  echo "Usage: DDEX_LICENSE_ACCEPTED=yes $0 OUTPUT_DIRECTORY" >&2
  exit 2
fi

case "$output_dir" in
  /|"$HOME"|"${HOME}/")
    echo "Refusing a broad output directory" >&2
    exit 2
    ;;
esac

archive_url='https://service.ddex.net/doc/Standards/ERN432/ERN-3305%20-%20ERN%20Part%201%20Definition%20of%20messages%20v4.3.2%20XSD.zip'
expected_archive_sha256='bbd5012204ea3dbf08025e58768570650d9f65775b0022f5a93770c0dd411938'
temporary_dir=$(mktemp -d "${TMPDIR:-/tmp}/tdf-ddex-schema.XXXXXX")
archive_path="$temporary_dir/ern432.zip"

cleanup() {
  rm -rf "$temporary_dir"
}
trap cleanup EXIT INT TERM

curl -fL --proto '=https' --tlsv1.2 "$archive_url" -o "$archive_path"
actual_sha256=$(shasum -a 256 "$archive_path" | awk '{print $1}')
if [ "$actual_sha256" != "$expected_archive_sha256" ]; then
  echo "Official DDEX schema archive checksum changed; expected $expected_archive_sha256, got $actual_sha256" >&2
  exit 1
fi

mkdir -p "$output_dir"
unzip -q "$archive_path" -d "$temporary_dir/extracted"
release_schema=$(find "$temporary_dir/extracted" -type f -name release-notification.xsd -print -quit)
allowed_values_schema=$(find "$temporary_dir/extracted" -type f -name allowed-value-sets.xsd -print -quit)
if [ -z "$release_schema" ] || [ -z "$allowed_values_schema" ]; then
  echo "The official archive did not contain the expected ERN schemas" >&2
  exit 1
fi

cp "$release_schema" "$output_dir/release-notification.xsd"
cp "$allowed_values_schema" "$output_dir/allowed-value-sets.xsd"
echo "Installed ERN 4.3.2 XSD and AVS schema in $output_dir"


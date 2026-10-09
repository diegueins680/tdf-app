#!/bin/sh
set -eu
umask 077

schema_dir=${1:-}
xml_file=${2:-}
resource_root=${3:-}
output_zip=${4:-}
generated_by=${5:-}
canonical_snapshot_sha256=${6:-}

if [ -z "$canonical_snapshot_sha256" ] || [ -z "$generated_by" ]; then
  echo "Usage: $0 SCHEMA_DIR XML_FILE RESOURCE_ROOT OUTPUT_ZIP GENERATED_BY CANONICAL_SNAPSHOT_SHA256" >&2
  exit 2
fi
if ! printf '%s' "$canonical_snapshot_sha256" | grep -Eq '^[0-9a-f]{64}$'; then
  echo "CANONICAL_SNAPSHOT_SHA256 must be a lowercase SHA-256 digest" >&2
  exit 2
fi
if [ ! -f "$schema_dir/release-notification.xsd" ] || [ ! -f "$schema_dir/allowed-value-sets.xsd" ]; then
  echo "The pinned official ERN 4.3.2 schemas are missing" >&2
  exit 2
fi
if [ ! -f "$xml_file" ] || [ ! -d "$resource_root" ] || [ -L "$xml_file" ] || [ -L "$resource_root" ]; then
  echo "XML_FILE and RESOURCE_ROOT must exist" >&2
  exit 2
fi
case "$output_zip" in
  /*) ;;
  *) output_zip="$(pwd)/$output_zip" ;;
esac
if [ -e "$output_zip" ] || [ -L "$output_zip" ]; then
  echo "Refusing to overwrite existing package: $output_zip" >&2
  exit 2
fi

# Validate the actual runtime files, not merely the existence of a schema dir.
[ "$(shasum -a 256 "$schema_dir/release-notification.xsd" | awk '{print $1}')" = def25b4e72696c9bbc1fed84962acc3a9bae2bc92ef25f8393c99b362aa53a6a ] &&
  [ "$(shasum -a 256 "$schema_dir/allowed-value-sets.xsd" | awk '{print $1}')" = 87e99fe74f57a640dce0d3247d16b3b52358562c1dbefc4617eb8a9b7360d943 ] || {
    echo 'Pinned ERN/AVS schema checksum mismatch' >&2; exit 1;
  }
# ERN generated here never needs DTDs/entities. Reject before any XML parsing;
# --nonet alone does not prevent local-file external entities.
[ "$(LC_ALL=C head -n 1 "$xml_file")" = '<?xml version="1.0" encoding="UTF-8"?>' ] || {
  echo 'Export XML must use the UTF-8 XML declaration emitted by the renderer' >&2; exit 1;
}
if LC_ALL=C grep -Eq '<!(DOCTYPE|ENTITY)' "$xml_file"; then
  echo 'DTD/entity declarations are not allowed in export XML' >&2; exit 1
fi
xmllint --nonet --noout --schema "$schema_dir/release-notification.xsd" "$xml_file"

# One main release ID, never an internal UUID or a fabricated fallback.
id_xpath='/*[local-name()="NewReleaseMessage"]/ReleaseList/Release/ReleaseId/*[self::ICPN or self::GRid]'
[ "$(xmllint --nonet --xpath "count($id_xpath)" "$xml_file")" = 1 ] || {
  echo 'releaseIdentifier: exactly one main ICPN or GRid is required for file naming' >&2; exit 1;
}
release_id=$(xmllint --nonet --xpath "string($id_xpath)" "$xml_file")
if ! printf '%s' "$release_id" | LC_ALL=C grep -Eq '^([0-9]{12,13}|[A-Z0-9]{18})$'; then
  echo 'releaseIdentifier: use the normalized provided ICPN or GRid; unsafe file name' >&2; exit 1
fi
message_file="$release_id.xml"

output_parent=$(dirname "$output_zip")
mkdir -p "$output_parent"
temporary_dir=$(mktemp -d "$output_parent/.tdf-ddex-package.XXXXXX")
package_dir="$temporary_dir/package"
mkdir -p "$package_dir/resources"

cleanup() {
  rm -rf "$temporary_dir"
}
trap cleanup EXIT INT TERM

cp "$xml_file" "$package_dir/$message_file"
# Reject symlinks even if unreferenced. Only copy explicitly referenced regular
# files; an unrelated master/private sidecar must never enter an export.
if [ -n "$(find "$resource_root" -type l -print)" ]; then
  echo 'Symlink resources are not allowed' >&2; exit 1
fi
uri_count=$(xmllint --nonet --xpath 'count(//*[local-name()="URI"])' "$xml_file")
case "$uri_count" in ''|*[!0-9]*) exit 1 ;; esac
[ "$uri_count" -gt 0 ] && [ "$uri_count" -le 10000 ] || { echo 'Unexpected resource URI count' >&2; exit 1; }
i=1
while [ "$i" -le "$uri_count" ]; do
  uri=$(xmllint --nonet --xpath "string((//*[local-name()='URI'])[$i])" "$xml_file")
  case "$uri" in
    resources/*) ;;
    *) echo 'Export URI must be a local resources/ path' >&2; exit 1 ;;
  esac
  case "$uri" in
    *[!a-zA-Z0-9_./-]*|*//*|*/../*|*/..|*/./*|*/.|*/)
      echo "Unsafe DDEX resource URI: $uri" >&2
      exit 1
      ;;
  esac
  # Match the technical anchor of this exact file, not another resource.
  technical_ref=$(xmllint --nonet --xpath "string((//*[local-name()='URI'])[$i]/ancestor::TechnicalDetails/TechnicalResourceDetailsReference)" "$xml_file")
  if ! printf '%s' "$technical_ref" | LC_ALL=C grep -Eq '^T[A-Za-z0-9-]+$'; then
    echo 'resources.technicalReference: missing or unsafe technical reference' >&2; exit 1
  fi
  if [ "$(xmllint --nonet --xpath "count((//*[local-name()='URI'])[$i]/ancestor::SoundRecording)" "$xml_file")" = 1 ]; then
    resource_type=SoundRecording
    case "$uri" in *.m4a|*.wav|*.flac|*.aiff) ;; *) echo 'resources.extension: unsupported audio extension' >&2; exit 1 ;; esac
  elif [ "$(xmllint --nonet --xpath "count((//*[local-name()='URI'])[$i]/ancestor::Image)" "$xml_file")" = 1 ]; then
    resource_type=CoverArt
    case "$uri" in *.jpg|*.jpeg|*.png) ;; *) echo 'resources.extension: unsupported image extension' >&2; exit 1 ;; esac
  else
    echo 'resources.type: only sound recordings and cover images are supported' >&2; exit 1
  fi
  [ "$uri" = "resources/${release_id}_${technical_ref}_${resource_type}.${uri##*.}" ] || {
    echo 'resources.fileName: expected ReleaseId_TechnicalResourceId_ResourceType.Ext matching the XML' >&2; exit 1;
  }
  if [ ! -f "$resource_root/${uri#resources/}" ]; then
    echo "DDEX XML references a missing resource: $uri" >&2
    exit 1
  fi
  mkdir -p "$(dirname "$package_dir/$uri")"
  cp "$resource_root/${uri#resources/}" "$package_dir/$uri"
  i=$((i + 1))
done

(
  cd "$package_dir"
  # The root manifest does not exist yet. Include even a referenced resource
  # named manifest.json so every packaged resource has a checksum.
  find . -type f -print \
    | LC_ALL=C sort \
    | while IFS= read -r relative_file; do
        digest=$(shasum -a 256 "$relative_file" | awk '{print $1}')
        bytes=$(wc -c < "$relative_file" | tr -d ' ')
        jq -nc --arg path "${relative_file#./}" --arg sha256 "$digest" --argjson bytes "$bytes" \
          '{path:$path,sha256:$sha256,bytes:$bytes}'
      done > "$temporary_dir/files.ndjson"
)

# Creation time belongs to the frozen ERN message. Wall-clock generation time
# is recorded by the export row, not injected into otherwise identical bytes.
generated_at=$(xmllint --nonet --xpath 'string(//*[local-name()="MessageHeader"]/*[local-name()="MessageCreatedDateTime"])' "$xml_file")
files_json=$(jq -scS '.' "$temporary_dir/files.ndjson")
content_hash=$(printf '%s' "$files_json" | shasum -a 256 | awk '{print $1}')
jq -n \
  --arg standard 'ERN' \
  --arg ernVersion '4.3.2' \
  --arg releaseProfile 'Audio' \
  --arg releaseProfileVersion '2.3.1' \
  --arg avsVersion '011' \
  --arg structuralDictionary 'DD-ERN-432' \
  --arg adapterVersion 'tdf-ern432-audio-v5' \
  --arg messageFile "$message_file" \
  --arg choreography 'Cloud Storage 1.8.1' \
  --arg generatedAt "$generated_at" \
  --arg generatedBy "$generated_by" \
  --arg canonicalSnapshotSha256 "$canonical_snapshot_sha256" \
  --arg contentSha256 "$content_hash" \
  --argjson files "$files_json" \
  '{manifestVersion:3,messageFile:$messageFile,packageFormat:"tdf-offline-review-bundle",deliveryPerformed:false,adapterVersion:$adapterVersion,standard:$standard,ernVersion:$ernVersion,releaseProfile:$releaseProfile,releaseProfileVersion:$releaseProfileVersion,businessProfileVersion:null,avsVersion:$avsVersion,structuralDictionary:$structuralDictionary,targetChoreography:$choreography,messageCreatedAt:$generatedAt,generatedBy:$generatedBy,canonicalSnapshotSha256:$canonicalSnapshotSha256,contentSha256:$contentSha256,validation:{xsd:"passed",fileNaming:"Cloud Storage 1.8.1 clause 5.3 (TDF subset)",serverLayout:"not-performed",schemaSha256:"def25b4e72696c9bbc1fed84962acc3a9bae2bc92ef25f8393c99b362aa53a6a",avsSha256:"87e99fe74f57a640dce0d3247d16b3b52358562c1dbefc4617eb8a9b7360d943",recipientAcceptance:"not-verified"},files:$files}' \
  > "$package_dir/manifest.json"

# Normalise metadata and ordering: identical inputs produce identical archives
# with this pinned toolchain, irrespective of temporary path or file mtime.
(
  cd "$package_dir"
  find . -type f -exec chmod 0644 {} +
  TZ=UTC find . -type f -exec touch -t 198001010000.00 {} +
  find . -type f -print | LC_ALL=C sort | sed 's|^./||' > "$temporary_dir/zip-files.txt"
  TZ=UTC zip -X -q "$temporary_dir/package.zip" -@ < "$temporary_dir/zip-files.txt"
)
# Publish without overwriting even if another process won after our first check.
ln "$temporary_dir/package.zip" "$output_zip"
package_sha256=$(shasum -a 256 "$output_zip" | awk '{print $1}')
echo "$package_sha256  $output_zip"

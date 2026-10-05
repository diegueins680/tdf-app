#!/bin/sh

# The caller supplies the kernel mount table and the fixed runtime target.
# A directory in the container's writable layer is not persistent storage.
require_private_upload_mount() {
  upload_mount_table=$1
  upload_mount_target=$2
  if [ ! -r "$upload_mount_table" ] || [ ! -d "$upload_mount_target" ] || [ ! -w "$upload_mount_target" ]; then
    return 1
  fi
  awk -v target="$upload_mount_target" '
    $5 == target {
      matches++
      count = split($6, options, ",")
      for (option_index = 1; option_index <= count; option_index++) if (options[option_index] == "rw") writable = 1
      filesystem = ""
      for (field = 7; field < NF; field++) if ($field == "-") { filesystem = $(field + 1); break }
      if (filesystem == "" || filesystem ~ /^(tmpfs|ramfs|overlay|aufs|proc|sysfs|devtmpfs)$/) volatile = 1
    }
    END { exit !(matches == 1 && writable == 1 && volatile != 1) }
  ' "$upload_mount_table"
}

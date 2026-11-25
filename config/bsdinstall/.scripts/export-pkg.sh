#!/bin/sh
# FreeBSD

set -e

[ "$#" -eq 1 ] || { echo "Usage: $0 <output-directory>" >&2; exit 1; }
output_dir=$1
query=$(mktemp "$output_dir/pkg-query.XXXXXX")
trap 'rm -f "$query"' 0
trap 'exit 1' HUP INT TERM

pkg query -e '%a = 0' '%R %o' > "$query"
awk '$1 == "FreeBSD-ports" {print $2}' "$query" > "$output_dir/pkglist.txt"
awk '$1 != "FreeBSD-ports" && $1 != "FreeBSD-base" {print $2}' "$query" > "$output_dir/foreignpkglist.txt"

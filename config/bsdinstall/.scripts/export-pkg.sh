#!/bin/sh
# FreeBSD

set -e

[ "$#" -eq 1 ] || { echo "Usage: $0 <host-config>" >&2; exit 1; }
profile=$1
[ -f "$profile" ] && [ ! -L "$profile" ] || {
    echo "Error: '$profile' must be a regular host configuration file" >&2
    exit 1
}

. "$HOME/repos/dotfiles/config/common/.scripts/update-config.sh"

workdir=$(mktemp -d "${profile}.export.XXXXXX")
cleanup() {
    rm -f "$workdir/query" "$workdir/pkglist.txt" "$workdir/foreignpkglist.txt" "$workdir/profile"
    rmdir "$workdir"
}
trap cleanup 0
trap 'exit 1' HUP INT TERM

pkg query -e '%a = 0' '%R %o' > "$workdir/query"
awk '$1 == "FreeBSD-ports" {print $2}' "$workdir/query" > "$workdir/pkglist.txt"
awk '$1 != "FreeBSD-ports" && $1 != "FreeBSD-base" {print $2}' "$workdir/query" > "$workdir/foreignpkglist.txt"

cp -p "$profile" "$workdir/profile"
update_config_section "$workdir/profile" pkg_packages "$workdir/pkglist.txt"
update_config_section "$workdir/profile" foreign_packages "$workdir/foreignpkglist.txt"
mv -f "$workdir/profile" "$profile"
echo "Updated package lists in $profile"

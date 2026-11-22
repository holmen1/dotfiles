#!/bin/sh

set -e

DOTFILES_DIR=$HOME/repos/dotfiles
HOSTNAME=$(hostname -s)
OS=$(uname -s)

case "$OS" in
    FreeBSD)
        PROFILE=$DOTFILES_DIR/install/hosts/FreeBSD/$HOSTNAME.conf
        EXPORTER=$DOTFILES_DIR/config/bsdinstall/.scripts/export-pkg.sh
        ;;
    *)
        echo "Error: package export is not supported on '$OS'" >&2
        exit 1
        ;;
esac

[ -f "$PROFILE" ] && [ ! -L "$PROFILE" ] || {
    echo "Error: '$PROFILE' must be a regular host configuration file" >&2
    exit 1
}
unset profile_hostname profile_os
. "$PROFILE"
if [ "${profile_hostname-}" != "$HOSTNAME" ] || [ "${profile_os-}" != "$OS" ]; then
    echo "Error: hostname or OS in '$PROFILE' does not match this system" >&2
    exit 1
fi

. "$DOTFILES_DIR/config/common/.scripts/update-config.sh"

workdir=$(mktemp -d "${PROFILE}.export.XXXXXX")
cleanup() {
    rm -f "$workdir/pkglist.txt" "$workdir/foreignpkglist.txt" "$workdir/profile"
    rmdir "$workdir"
}
trap cleanup 0
trap 'exit 1' HUP INT TERM

"$EXPORTER" "$workdir"
cp -p "$PROFILE" "$workdir/profile"
update_config_section "$workdir/profile" pkg_packages "$workdir/pkglist.txt"
update_config_section "$workdir/profile" foreign_packages "$workdir/foreignpkglist.txt"
mv -f "$workdir/profile" "$PROFILE"
echo "Updated package lists in $PROFILE"

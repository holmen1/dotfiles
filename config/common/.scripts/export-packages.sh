#!/bin/sh

set -e

DOTFILES_DIR=$HOME/repos/dotfiles
. "$DOTFILES_DIR/install/hosts/scripts/host-profile.sh"
if ! select_host; then
    echo "Error: package export is not supported on this OS or distribution" >&2
    exit 1
fi

case "$OS" in
    Linux)
        EXPORTER=$OS_SCRIPTS_DIR/export-pacman.sh
        ;;
    FreeBSD)
        EXPORTER=$OS_SCRIPTS_DIR/export-pkg.sh
        ;;
esac

PROFILE=$HOST_CONFIG
[ -f "$PROFILE" ] && [ ! -L "$PROFILE" ] || {
    echo "Error: '$PROFILE' must be a regular host configuration file" >&2
    exit 1
}
load_host_profile

. "$DOTFILES_DIR/install/hosts/scripts/update-config.sh"

workdir=$(mktemp -d "${PROFILE}.export.XXXXXX")
cleanup() {
    rm -f "$workdir/pkglist.txt" "$workdir/foreignpkglist.txt" "$workdir/profile" "$workdir/pacman.err"
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

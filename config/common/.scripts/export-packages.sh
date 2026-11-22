#!/bin/sh

set -e

DOTFILES_DIR=$HOME/repos/dotfiles
. "$DOTFILES_DIR/config/common/.scripts/host-profile.sh"
if ! select_host; then
    echo "Error: package export is not supported on this OS or distribution" >&2
    exit 1
fi

case "$OS" in
    FreeBSD)
        EXPORTER=$DOTFILES_DIR/config/bsdinstall/.scripts/export-pkg.sh
        ;;
esac

PROFILE=$HOST_CONFIG
[ -f "$PROFILE" ] && [ ! -L "$PROFILE" ] || {
    echo "Error: '$PROFILE' must be a regular host configuration file" >&2
    exit 1
}
load_host_profile

. "$DOTFILES_DIR/config/common/.scripts/update-config.sh"

workdir=$(mktemp -d "${PROFILE}.export.XXXXXX")
cleanup() {
    rm -f "$workdir/pkglist.txt" "$workdir/foreignpkglist.txt" "$workdir/profile" "$workdir/pacman.err"
    rmdir "$workdir"
}
trap cleanup 0
trap 'exit 1' HUP INT TERM

if [ "$OS" = FreeBSD ]; then
    "$EXPORTER" "$workdir"
else
    query_pacman() {
        if pacman "$1" > "$2" 2> "$workdir/pacman.err"; then
            cat "$workdir/pacman.err" >&2
        else
            status=$?
            # Pacman returns 1 without diagnostics when no packages match.
            if [ "$status" -eq 1 ] && [ ! -s "$2" ] && [ ! -s "$workdir/pacman.err" ]; then
                return 0
            fi
            cat "$workdir/pacman.err" >&2
            echo "Error: pacman $1 failed" >&2
            return 1
        fi
    }
    query_pacman -Qqen "$workdir/pkglist.txt"
    query_pacman -Qqem "$workdir/foreignpkglist.txt"
fi
cp -p "$PROFILE" "$workdir/profile"
update_config_section "$workdir/profile" pkg_packages "$workdir/pkglist.txt"
update_config_section "$workdir/profile" foreign_packages "$workdir/foreignpkglist.txt"
mv -f "$workdir/profile" "$PROFILE"
echo "Updated package lists in $PROFILE"

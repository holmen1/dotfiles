#!/bin/sh

set -e

usage() {
    echo "Usage: $0 [--pre|--packages|--foreign]" >&2
    exit 1
}

[ "$#" -le 1 ] || usage
case "${1---packages}" in
    --pre|--packages|--foreign) ;;
    *) usage ;;
esac

DOTFILES_DIR=$HOME/repos/dotfiles
. "$DOTFILES_DIR/config/common/.scripts/host-profile.sh"
if ! select_host; then
    echo "Error: package import is not supported on this OS or distribution" >&2
    exit 1
fi

case "$OS" in
    FreeBSD) INSTALLER=$DOTFILES_DIR/config/bsdinstall/.scripts/install-pkg.sh ;;
    *)
        echo "Error: package import is not supported on '$OS'" >&2
        exit 1
        ;;
esac

load_host_profile
exec "$INSTALLER" --profile "$HOST_CONFIG" "${1---packages}"

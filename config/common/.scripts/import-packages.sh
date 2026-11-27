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

load_host_profile

case "$OS" in
    FreeBSD)
        exec "$DOTFILES_DIR/config/bsdinstall/.scripts/install-pkg.sh" \
            --profile "$HOST_CONFIG" "${1---packages}"
        ;;
    Linux)
        case "$1" in
            --pre)
                [ "${pkg_pre+x}" = x ] || {
                    echo "Error: pkg_pre missing from '$HOST_CONFIG'" >&2
                    exit 1
                }
                package_list=$pkg_pre
                ;;
            --packages)
                [ "${pkg_packages+x}" = x ] || {
                    echo "Error: pkg_packages missing from '$HOST_CONFIG'" >&2
                    exit 1
                }
                package_list=$pkg_packages
                ;;
            --foreign)
                [ "${foreign_packages+x}" = x ] || {
                    echo "Error: foreign_packages missing from '$HOST_CONFIG'" >&2
                    exit 1
                }
                package_list=$foreign_packages
                ;;
            *) package_list=$pkg_packages ;;
        esac
        case "$package_list" in
            *[![:space:]]*) ;;
            *)
                echo "No packages in selected group; skipping."
                exit 0
                ;;
        esac
        printf '%s\n' "$package_list" |
            "$DOTFILES_DIR/config/artixinstall/.scripts/install-pacman.sh"
        ;;
    *)
        echo "Error: package import is not supported on '$OS'" >&2
        exit 1
        ;;
esac

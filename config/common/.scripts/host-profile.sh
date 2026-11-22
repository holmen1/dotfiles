#!/bin/sh

select_host() {
    HOSTNAME=$(hostname -s) || return 1
    OS=$(uname -s) || return 1

    case "$OS" in
        FreeBSD) ;;
        Linux)
            release_file=${1:-/etc/os-release}
            [ -r "$release_file" ] || return 1
            ( . "$release_file"; [ "${ID-}" = artix ] ) || return 1
            ;;
        *) return 1 ;;
    esac
    HOST_CONFIG=$HOME/repos/dotfiles/install/hosts/$OS/$HOSTNAME.conf
}

load_host_profile() {
    if [ ! -f "$HOST_CONFIG" ]; then
        echo "Error: host configuration '$HOST_CONFIG' not found" >&2
        return 1
    fi
    unset profile_hostname profile_os profile_distribution terminal window_manager \
        xkb_layout services packages pkg_packages foreign_packages ghc_version \
        xmonad_version xmonad_contrib_version
    . "$HOST_CONFIG" || return 1
    if [ "${profile_hostname-}" != "$HOSTNAME" ] || [ "${profile_os-}" != "$OS" ] ||
       { [ "$OS" = Linux ] && [ "${profile_distribution-}" != artix ]; }; then
        echo "Error: hostname or OS in '$HOST_CONFIG' does not match this system" >&2
        return 1
    fi
}

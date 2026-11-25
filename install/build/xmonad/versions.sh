#!/bin/sh

XMONAD_VER=0.18.1
XMONAD_CONTRIB_VER=0.18.2
GHC_EXPECTED=

. "$HOME/repos/dotfiles/config/common/.scripts/host-profile.sh"
if select_host && [ -f "$HOST_CONFIG" ]; then
    load_host_profile
    if [ "${window_manager-}" = xmonad ]; then
        if [ -z "${ghc_version-}" ] || [ -z "${xmonad_version-}" ] ||
           [ -z "${xmonad_contrib_version-}" ]; then
            echo "Error: XMonad versions missing from '$HOST_CONFIG'" >&2
            exit 1
        fi
        GHC_EXPECTED=$ghc_version
        XMONAD_VER=$xmonad_version
        XMONAD_CONTRIB_VER=$xmonad_contrib_version
    fi
fi

check_ghc_version() {
    if ! GHC_PATH=$(command -v ghc); then
        echo "Error: no ghc found on PATH" >&2
        return 1
    fi
    GHC_VER=$("$GHC_PATH" --numeric-version) || return 1
    if [ -n "$GHC_EXPECTED" ] && [ "$GHC_VER" != "$GHC_EXPECTED" ]; then
        echo "Error: expected GHC $GHC_EXPECTED, found $GHC_VER" >&2
        return 1
    fi
    echo "Using ghc-$GHC_VER"
}

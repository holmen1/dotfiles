#!/bin/sh

set -e

DOTFILES_DIR=$HOME/repos/dotfiles
CONFIG_DIR=$DOTFILES_DIR/config
COMMON_DIR=$CONFIG_DIR/common/.scripts
HOST_SCRIPTS_DIR=$DOTFILES_DIR/install/hosts/scripts
BUILD_DIR=$DOTFILES_DIR/install/build

. "$HOST_SCRIPTS_DIR/host-profile.sh"
if ! select_host; then
    echo "Error: setup is not supported on this OS or distribution" >&2
    exit 1
fi
load_host_profile
if [ -z "${terminal-}" ] || [ -z "${window_manager-}" ] || [ -z "${xkb_layout-}" ] ||
   [ -z "${packages-}" ] || [ "${pkg_packages+x}" != x ]; then
    echo "Error: invalid host configuration '$HOST_CONFIG'" >&2
    exit 1
fi
if [ "$OS" = Linux ] && [ "$window_manager" = xmonad ] &&
   { [ -z "${ghc_version-}" ] || [ -z "${xmonad_version-}" ] ||
     [ -z "${xmonad_contrib_version-}" ]; }; then
    echo "Error: XMonad versions missing from '$HOST_CONFIG'" >&2
    exit 1
fi

prompt() {
    printf '%s' "$1"
    IFS= read -r ans || ans=
}

echo "$HOST_CONFIG"
prompt "Install package list? [y/N] "
case "$ans" in
    [Yy]*) "$COMMON_DIR/import-packages.sh" --packages ;;
esac

prompt "Install foreign packages? [y/N] "
case "$ans" in
    [Yy]*) "$COMMON_DIR/import-packages.sh" --foreign ;;
esac

prompt "Link dotfiles? [y/N] "
case "$ans" in
    [Yy]*) "$HOST_SCRIPTS_DIR/link_config.sh" "$HOST_CONFIG" ;;
esac

prompt "Build $window_manager? [y/N] "
case "$ans" in
    [Yy]*)
        if [ "$window_manager" = xmonad ]; then
            "$BUILD_DIR/xmonad/build-custom-xmonad.sh"
        else
            "$BUILD_DIR/$window_manager/build-$window_manager.sh"
        fi
        ;;
esac

prompt "Install $window_manager? [y/N] "
case "$ans" in
    [Yy]*)
        if [ "$window_manager" = xmonad ]; then
            "$BUILD_DIR/xmonad/install-custom-xmonad.sh"
        else
            "$BUILD_DIR/$window_manager/install-$window_manager.sh"
        fi
        ;;
esac
sudo -k

prompt "Build xkb keymap? [y/N] "
case "$ans" in
    [Yy]*)
        "$BUILD_DIR/xkb/build-xkb.sh" "$xkb_layout"
        echo "Built xkb keymap"
        ;;
esac

prompt "Link menu and xkbtoggle? [y/N] "
case "$ans" in
    [Yy]*) "$HOST_SCRIPTS_DIR/link-system-commands.sh" ;;
esac

if [ -n "${services-}" ]; then
    prompt "Enable services? [y/N] "
    case "$ans" in
        [Yy]*) "$HOST_SCRIPTS_DIR/enable-services.sh" ;;
    esac
fi

prompt "Run tests? [Y/n] "
case "$ans" in
    [Nn]) ;;
    *) "$COMMON_DIR/sanity-check.sh" ;;
esac

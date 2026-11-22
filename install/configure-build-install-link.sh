#!/bin/sh

set -e

DOTFILES_DIR=$HOME/repos/dotfiles
CONFIG_DIR=$DOTFILES_DIR/config
COMMON_DIR=$CONFIG_DIR/common/.scripts
BUILD_DIR=$DOTFILES_DIR/install/build
HOSTNAME=$(hostname -s)
OS=$(uname -s)
HOST_CONFIG=$DOTFILES_DIR/install/hosts/$OS/$HOSTNAME.conf

if [ "$OS" != FreeBSD ]; then
    echo "Error: setup is not supported on '$OS'" >&2
    exit 1
fi
if [ ! -f "$HOST_CONFIG" ]; then
    echo "Error: host configuration '$HOST_CONFIG' not found" >&2
    exit 1
fi

unset profile_hostname profile_os terminal window_manager xkb_layout packages pkg_packages
. "$HOST_CONFIG"
if [ "${profile_hostname-}" != "$HOSTNAME" ] || [ "${profile_os-}" != "$OS" ] ||
   [ -z "${terminal-}" ] || [ -z "${window_manager-}" ] || [ -z "${xkb_layout-}" ] ||
   [ -z "${packages-}" ] || [ "${pkg_packages+x}" != x ]; then
    echo "Error: invalid host configuration '$HOST_CONFIG'" >&2
    exit 1
fi

prompt() {
    printf '%s' "$1"
    IFS= read -r ans || ans=
}

prompt "Configure git? [y/N] "
case "$ans" in
    [Yy]*) "$COMMON_DIR/configure-git.sh" ;;
esac

prompt "Generate SSH key? [y/N] "
case "$ans" in
    [Yy]*) "$COMMON_DIR/generate-ssh-key.sh" ;;
esac

echo "$HOST_CONFIG"
prompt "Install pkglist? [y/N] "
case "$ans" in
    [Yy]*) "$CONFIG_DIR/bsdinstall/.scripts/install-pkg.sh" --profile "$HOST_CONFIG" ;;
esac

prompt "Build $terminal? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/$terminal/build-$terminal.sh" ;;
esac

prompt "Install $terminal? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/$terminal/install-$terminal.sh" ;;
esac

prompt "Build $window_manager? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/$window_manager/build-$window_manager.sh" ;;
esac

prompt "Install $window_manager? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/$window_manager/install-$window_manager.sh" ;;
esac
sudo -k

prompt "Build xkb keymap? [y/N] "
case "$ans" in
    [Yy]*)
        "$BUILD_DIR/xkb/build-xkb.sh" "$xkb_layout"
        echo "Built xkb keymap"
        ;;
esac

prompt "Link dotfiles? [y/N] "
case "$ans" in
    [Yy]*) "$COMMON_DIR/link_config.sh" "$HOST_CONFIG" ;;
esac

prompt "Link menu and xkbtoggle? [y/N] "
case "$ans" in
    [Yy]*)
        sudo ln -sf "$CONFIG_DIR/common/.scripts/dmenu-menu.sh" /usr/local/bin/dmenu-menu
        sudo ln -sf "$CONFIG_DIR/xkb/.scripts/xkb-toggle.sh" /usr/local/bin/xkb-toggle
        [ -L /usr/local/bin/dmenu-menu ] && echo "dmenu-menu linked"
        [ -L /usr/local/bin/xkb-toggle ] && echo "xkb-toggle linked"
        ;;
esac

prompt "Run tests? [Y/n] "
case "$ans" in
    [Nn]) ;;
    *) "$DOTFILES_DIR/install/sanity-check.sh" ;;
esac

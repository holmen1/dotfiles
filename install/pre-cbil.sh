#!/bin/sh

set -e

DOTFILES_DIR=$HOME/repos/dotfiles
CONFIG_DIR=$DOTFILES_DIR/config
COMMON_DIR=$CONFIG_DIR/common/.scripts
BUILD_DIR=$DOTFILES_DIR/install/build

. "$COMMON_DIR/host-profile.sh"
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
prompt "Install pre-pkglist? [y/N] "
case "$ans" in
    [Yy]*)
        if [ "$OS" = FreeBSD ]; then
            "$COMMON_DIR/import-packages.sh" --pre
        else
            printf '%s\n' "$pkg_packages" | "$CONFIG_DIR/artixinstall/.scripts/install-pacman.sh"
        fi
        ;;
esac

prompt "Build $terminal? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/$terminal/build-$terminal.sh" ;;
esac

prompt "Install $terminal? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/$terminal/install-$terminal.sh" ;;
esac


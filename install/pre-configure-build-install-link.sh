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
if [ "$OS" = Linux ] && [ -z "${ghc_version-}" ]; then
    echo "Error: GHC version missing from '$HOST_CONFIG'" >&2
    exit 1
fi
if [ "$OS" = Linux ] && [ "$window_manager" = xmonad ] &&
   { [ -z "${xmonad_version-}" ] || [ -z "${xmonad_contrib_version-}" ]; }; then
    echo "Error: XMonad versions missing from '$HOST_CONFIG'" >&2
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
if [ "$OS" = FreeBSD ]; then
    prompt "Install pre-pkglist? [y/N] "
else
    prompt "Install pkglist? [y/N] "
fi
case "$ans" in
    [Yy]*)
        if [ "$OS" = FreeBSD ]; then
            "$COMMON_DIR/import-packages.sh" --pre
        else
            "$COMMON_DIR/import-packages.sh" --packages
        fi
        ;;
esac

if [ "$OS" = FreeBSD ]; then
    prompt "Install GHC using pkg? [y/N] "
else
    prompt "Install GHC $ghc_version? [y/N] "
fi
case "$ans" in
    [Yy]*)
        if [ "$OS" = FreeBSD ]; then
            if pkg query '%n' lang/ghc >/dev/null 2>&1; then
                echo "lang/ghc is already installed, skipping."
            else
                sudo pkg install -y lang/ghc
                sudo -k
            fi
        else
            "$BUILD_DIR/ghc/build-ghc.sh" "$ghc_version"
        fi
        ;;
esac

prompt "Build xmonad libraries? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/xmonad/build-xmonad-libs.sh" ;;
esac

prompt "Build $terminal? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/$terminal/build-$terminal.sh" ;;
esac

prompt "Install $terminal? [y/N] "
case "$ans" in
    [Yy]*) "$BUILD_DIR/$terminal/install-$terminal.sh" ;;
esac

"$DOTFILES_DIR/install/sanity-pre-check.sh"

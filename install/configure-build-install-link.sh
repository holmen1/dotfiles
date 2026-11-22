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
    [Yy]*)
        if [ "$OS" = FreeBSD ]; then
            "$CONFIG_DIR/bsdinstall/.scripts/install-pkg.sh" --profile "$HOST_CONFIG"
        else
            set -f
            for package in $pkg_packages; do
                if ! pacman -Q "$package" >/dev/null 2>&1; then
                    sudo pacman -S --noconfirm "$package"
                fi
            done
            set +f
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

if [ "$OS" = Linux ]; then
    prompt "Link dotfiles? [y/N] "
    case "$ans" in
        [Yy]*) "$COMMON_DIR/link_config.sh" "$HOST_CONFIG" ;;
    esac

    if [ "$window_manager" = xmonad ]; then
        prompt "Install GHC $ghc_version? [y/N] "
        case "$ans" in
            [Yy]*) "$BUILD_DIR/ghc/build-ghc.sh" "$ghc_version" ;;
        esac

        prompt "Build xmonad libraries? [y/N] "
        case "$ans" in
            [Yy]*) "$BUILD_DIR/xmonad/build-xmonad-libs.sh" ;;
        esac
    fi
fi

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

if [ "$OS" = FreeBSD ]; then
    prompt "Link dotfiles? [y/N] "
    case "$ans" in
        [Yy]*) "$COMMON_DIR/link_config.sh" "$HOST_CONFIG" ;;
    esac
fi

prompt "Link menu and xkbtoggle? [y/N] "
case "$ans" in
    [Yy]*)
        sudo ln -sf "$CONFIG_DIR/common/.scripts/dmenu-menu.sh" /usr/local/bin/dmenu-menu
        sudo ln -sf "$CONFIG_DIR/xkb/.scripts/xkb-toggle.sh" /usr/local/bin/xkb-toggle
        [ -L /usr/local/bin/dmenu-menu ] && echo "dmenu-menu linked"
        [ -L /usr/local/bin/xkb-toggle ] && echo "xkb-toggle linked"
        ;;
esac

if [ "$OS" = Linux ]; then
    prompt "Enable services? [y/N] "
    case "$ans" in
        [Yy]*)
            for service in iwd dbus elogind; do
                sudo rc-update add "$service" default
            done
            ;;
    esac
fi

prompt "Run tests? [Y/n] "
case "$ans" in
    [Nn]) ;;
    *) "$DOTFILES_DIR/install/sanity-check.sh" ;;
esac

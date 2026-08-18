#!/bin/sh

PROFILE=artix
DOTFILES_DIR=~/repos/dotfiles

CONFIG_DIR=$DOTFILES_DIR/config
COMMON_DIR=$CONFIG_DIR/common/.scripts
PROFILE_DIR=$DOTFILES_DIR/install/profiles/"$PROFILE"install
BUILD_DIR=$DOTFILES_DIR/install/build

COMPUTERNAME=$(hostname -s)
PKGPROFILE=${COMPUTERNAME}

LINK_SCRIPT=$COMMON_DIR/link_config.sh
LINKS=$PROFILE_DIR/links/$PKGPROFILE/links.config

INSTALL_SCRIPT=$CONFIG_DIR/artixinstall/.scripts/install-pacman.sh
PKGLIST=$PROFILE_DIR/packages/$PKGPROFILE/pkglist.txt

XMONAD_DIR=$BUILD_DIR/xmonad
ST_DIR=$BUILD_DIR/st
XKB_DIR=$BUILD_DIR/xkb

TEST=$PROFILE_DIR/tests/$PKGPROFILE/sanity_check.sh

read -p "Configure git? [y/N] " ans
case "$ans" in
    [Yy]*)
    $COMMON_DIR/configure-git.sh
    ;;
esac

read -p "Generate SSH key? [y/N] " ans
case "$ans" in
    [Yy]*)
    $COMMON_DIR/generate-ssh-key.sh
    ;;
esac

echo "$PKGLIST"
read -p "Install pkglist? [y/N] " ans
case "$ans" in
    [Yy]*)
    $INSTALL_SCRIPT $PKGLIST
    ;;
esac

read -p "Rebuild xmonad? [y/N] " ans
case "$ans" in
    [Yy]*)
    $XMONAD_DIR/build-custom-xmonad.sh
    ;;
esac

read -p "Install xmonad? [y/N] " ans
case "$ans" in
    [Yy]*)
    $XMONAD_DIR/install-custom-xmonad.sh
    ;;
esac

read -p "Build st? [y/N] " ans
case "$ans" in
    [Yy]*)
    $ST_DIR/build-st.sh
    ;;
esac

read -p "Install st? [y/N] " ans
case "$ans" in
    [Yy]*)
    $ST_DIR/install-st.sh
    ;;
esac

read -p "Build xkb keymap? [y/N] " ans
case "$ans" in
    [Yy]*)
    $XKB_DIR/build-xkb.sh
    echo "Built xkb keymap"
    ;;
esac

read -p "Link dotfiles? [y/N] " ans
case "$ans" in
    [Yy]*)
    $LINK_SCRIPT $LINKS
    ;;
esac

# TODO script
read -p "Link menu and xkbtoggle? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Used by xmonad
    sudo ln -sf "$CONFIG_DIR/common/.scripts/dmenu-menu.sh" /usr/local/bin/dmenu-menu
    sudo ln -sf "$CONFIG_DIR/xkb/.scripts/xkb-toggle.sh" /usr/local/bin/xkb-toggle
    [ -L /usr/local/bin/dmenu-menu ] && echo "dmenu-menu linked"
    [ -L /usr/local/bin/xkb-toggle ] && echo "xkb-toggle linked"
    ;;
esac
sudo -k

read -p "Enable services? [y/N] " ans
case "$ans" in
    [Yy]*)
    # OpenRC — add services at default runlevel
    sudo rc-update add iwd default
    sudo rc-update add dbus default
    sudo rc-update add elogind default
    echo "Services added to default runlevel"
    ;;
esac

read -p "Run tests? [Y/n] " ans
case "$ans" in
    [Nn]) ;;
    *)    $TEST ;;
esac

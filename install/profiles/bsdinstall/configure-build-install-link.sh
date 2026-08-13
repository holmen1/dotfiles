#!/bin/sh

PROFILE=bsd
DOTFILES_DIR=~/repos/dotfiles

CONFIG_DIR=$DOTFILES_DIR/config
COMMON_DIR=$CONFIG_DIR/common/.scripts
PROFILE_DIR=$DOTFILES_DIR/install/profiles/"$PROFILE"install
BUILD_DIR=$DOTFILES_DIR/install/build

COMPUTERNAME=$(hostname -s)
PKGPROFILE=${COMPUTERNAME}

LINK_SCRIPT=$COMMON_DIR/link_config.sh
LINKS=$PROFILE_DIR/links/$PKGPROFILE/links.config

INSTALL_SCRIPT=$CONFIG_DIR/"$PROFILE"install/.scripts/install-pkg.sh
PKGLIST=$PROFILE_DIR/packages/$PKGPROFILE/pkglist.txt

ST_DIR=$BUILD_DIR/st
DWM_DIR=$BUILD_DIR/dwm
XKB_DIR=$BUILD_DIR/xkb

TEST=$PROFILE_DIR/tests/$COMPUTERNAME/sanity_check.sh

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

# Install packages
echo "$PKGLIST"
read -p "Install pkglist? [y/N] " ans
case "$ans" in
    [Yy]*)
    $INSTALL_SCRIPT $PKGLIST
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

read -p "Build dwm? [y/N] " ans
case "$ans" in
    [Yy]*)
    $DWM_DIR/build-dwm.sh
    ;;
esac

read -p "Install dwm? [y/N] " ans
case "$ans" in
    [Yy]*)
    $DWM_DIR/install-dwm.sh
    ;;
esac
sudo -k

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
    # Link dotfiles
    $LINK_SCRIPT $LINKS
    ;;
esac

read -p "Link menu and xkbtoggle? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Used by dwm
    sudo ln -sf "$CONFIG_DIR/common/.scripts/dmenu-menu.sh" /usr/local/bin/dmenu-menu
    sudo ln -sf "$CONFIG_DIR/xkb/.scripts/xkb-toggle.sh" /usr/local/bin/xkb-toggle
    ;;
esac

read -p "Run tests? [Y/n] " ans
case "$ans" in
    [Nn])
    # Skip tests
    ;;
    *)
    # Test (default)
    $TEST
    ;;
esac


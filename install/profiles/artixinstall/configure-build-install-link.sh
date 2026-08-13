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
    $XMONAD_DIR/rebuild-xmonad.sh
    ;;
esac

read -p "Install xmonad? [y/N] " ans
case "$ans" in
    [Yy]*)
    sudo mkdir -p /opt/xmonad
    for file in /opt/xmonad/*; do
        case "$file" in
            *.bak) ;;
            *) [ -e "$file" ] && sudo mv "$file" "${file}.bak" ;;
        esac
    done
    sudo cp -f $XMONAD_DIR/bin/xmonad-0.18.[0-9] /opt/xmonad/
    LATEST_XMONAD=$(ls -v /opt/xmonad/xmonad-0.18.[0-9] | grep -v '\.bak$' | tail -n 1)
    if [ -n "$LATEST_XMONAD" ]; then
        sudo ln -sf "$LATEST_XMONAD" /usr/local/bin/xmonad
        echo "Created symlink for xmonad -> $LATEST_XMONAD"
    fi
    sudo -k
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

read -p "Link menu and xkbtoggle? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Used by dwm
    sudo ln -sf "$CONFIG_DIR/common/.scripts/dmenu-menu.sh" /usr/local/bin/dmenu-menu
    sudo ln -sf "$CONFIG_DIR/xkb/.scripts/xkb-toggle.sh" /usr/local/bin/xkb-toggle
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

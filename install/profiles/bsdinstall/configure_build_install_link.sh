#!/bin/sh

PROFILE=bsd
DOTFILES_DIR=~/repos/dotfiles

COMMON_DIR=$DOTFILES_DIR/install/common
PROFILE_DIR=$DOTFILES_DIR/install/profiles/"$PROFILE"install
BUILD_DIR=$DOTFILES_DIR/install/build

COMPUTERNAME=$(hostname -s)
PKGPROFILE=${COMPUTERNAME}

LINK_SCRIPT=$COMMON_DIR/link_config.sh
LINKS=$PROFILE_DIR/links/$PKGPROFILE/links.config

INSTALL_SCRIPT=$PROFILE_DIR/scripts/install-pkg.sh
PKGLIST=$PROFILE_DIR/packages/$PKGPROFILE/pkglist.txt
FPKGLIST=$PROFILE_DIR/packages/$PKGPROFILE/foreignpkglist.txt

XMONAD_DIR=$BUILD_DIR/xmonad
ST_DIR=$BUILD_DIR/st
XKB_DIR=$BUILD_DIR/xkb

TEST=$PROFILE_DIR/tests/sanity_check.sh

USER=$(whoami)
EMAIL=$USER@gmail.com

read -p "Configure git? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Configure git
    git config --global user.name "$USER"
    git config --global user.email "$EMAIL"
    ;;
esac

read -p "Generate SSH key? [y/N] " ans
case "$ans" in
    [Yy]*)
# Generate ssh key
    cd $HOME
    mkdir -p .ssh
    chmod 700 .ssh
    ssh-keygen -t ed25519 -C "$EMAIL" -f $HOME/.ssh/id_ed25519
    eval "$(ssh-agent -s)"
    ssh-add $HOME/.ssh/id_ed25519
    ;;
esac

# Install packages
echo "$PKGLIST"
read -p "Install pkglist? [y/N] " ans
case "$ans" in
    [Yy]*)
    $SCRIPTS_DIR/install-pkg.sh $PKGLIST
    ;;
esac

read -p "Build xmonad? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Build Xmonad
    $XMONAD_DIR/build-xmonad.sh
    ;;
esac

read -p "Install xmonad? [y/N] " ans
case "$ans" in
    [Yy]*)
    sudo mkdir -p /opt/xmonad
    sudo rm -f /opt/xmonad/*

    sudo cp -f $XMONAD_DIR/bin/xmonad-v0.18.[0-9] /opt/xmonad/
    echo "Installed xmonad to /opt/xmonad/"
    sudo ln -sf /opt/xmonad/xmonad-v0.18.* /usr/local/bin/xmonad
    echo "Created symlink for xmonad"
    ;;
esac
sudo -k

read -p "Build st? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Build st
    $ST_DIR/build-st.sh
    ;;
esac

read -p "Install st? [y/N] " ans
case "$ans" in
    [Yy]*)
    sudo mkdir -p /opt/st
    sudo rm -f /opt/st/*

    sudo cp -f $ST_DIR/bin/st-0.9.[0-9] /opt/st/
    echo "Installed st to /opt/st/"
    sudo ln -sf /opt/st/st-0.9.* /usr/local/bin/st
    sudo -k
    echo "Created symlink for st"
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

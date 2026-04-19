#!/bin/sh

# Debian install script with profile support (modeled after archinstall)

USER=$(whoami)
EMAIL=$USER@gmail.com

DOTFILES_DIR=~/repos/dotfiles
SCRIPTS_DIR=$DOTFILES_DIR/scripts
INSTALL_SCRIPT=$SCRIPTS_DIR/install-apt.sh
LINK_SCRIPT=$SCRIPTS_DIR/link_config.sh

XMONAD_DIR=$DOTFILES_DIR/install/build/xmonad
ST_DIR=$DOTFILES_DIR/install/build/st
XKB_DIR=$DOTFILES_DIR/install/build/xkb

COMPUTERNAME=$(hostnamectl --static 2>/dev/null || hostname -s)
PKGPROFILE=${COMPUTERNAME}
PKGLIST=$DOTFILES_DIR/install/debianinstall/packages/$PKGPROFILE/pkglist.txt
LINKS=$DOTFILES_DIR/install/debianinstall/links/$PKGPROFILE/links.config
TEST=$DOTFILES_DIR/install/debianinstall/tests/$PKGPROFILE/sanity_check.sh

read -p "Configure git? [y/N] " ans
case "$ans" in
    [Yy]*)
    git config --global user.name "$USER"
    git config --global user.email "$EMAIL"
    ;;
esac

read -p "Generate SSH key? [y/N] " ans
case "$ans" in
    [Yy]*)
    cd ~
    mkdir -p .ssh
    chmod 700 .ssh
    ssh-keygen -t ed25519 -C "$EMAIL" -f ~/.ssh/id_ed25519
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/id_ed25519
    ;;
esac

echo "$PKGLIST"
read -p "Install pkglist? [y/N] " ans
case "$ans" in
    [Yy]*)
    $INSTALL_SCRIPT $PKGLIST
    ;;
esac

read -p "Build xmonad? [y/N] " ans
case "$ans" in
    [Yy]*)
    $XMONAD_DIR/build-xmonad.sh
    ;;
esac

read -p "Install xmonad? [y/N] " ans
case "$ans" in
    [Yy]*)
    sudo mkdir -p /opt/xmonad
    for file in /opt/xmonad/*; do
        if [ -e "$file" ] && [[ ! "$file" == *.bak ]]; then
            sudo mv "$file" "${file}.bak"
        fi
    done
    sudo cp -f $XMONAD_DIR/bin/xmonad-v0.18.[0-9] /opt/xmonad/
    echo "Installed xmonad to /opt/xmonad/"
    LATEST_XMONAD=$(ls -v /opt/xmonad/xmonad-v0.18.[0-9] | grep -v "\.bak$" | tail -n 1)
    if [ -n "$LATEST_XMONAD" ]; then
        sudo ln -sf "$LATEST_XMONAD" /usr/local/bin/xmonad
        echo "Created symlink for xmonad pointing to $LATEST_XMONAD"
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
    sudo mkdir -p /opt/st
    sudo rm -f /opt/st/*
    sudo cp -f $ST_DIR/bin/st-0.9.[0-9] /opt/st/
    echo "Installed st to /opt/st/"
    sudo ln -sf /opt/st/st-0.9.* /usr/local/bin/st
    sudo -k
    echo "Created symlink for st"
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

read -p "Enable services? [y/N] " ans
case "$ans" in
    [Yy]*)
    systemctl --user daemon-reload
    systemctl --user enable --now system-monitor.timer
    ;;
esac

read -p "Run tests? [Y/n] " ans
case "$ans" in
    [Nn])
    ;;
    *)
    # Run the Debian sanity check for this profile
    if [ -x "$TEST" ]; then
        "$TEST"
    else
        echo "Sanity check script not found or not executable: $TEST"
    fi
    ;;
esac


#!/bin/sh

USER=$(whoami)
EMAIL=$USER@gmail.com

DOTFILES_DIR=~/repos/dotfiles
SCRIPTS_DIR=$DOTFILES_DIR/scripts
LINK_SCRIPT=$SCRIPTS_DIR/link_config.sh
LINKS=$DOTFILES_DIR/install/bsdinstall/links/suckless_links.config

XMONAD_DIR=$DOTFILES_DIR/install/build/xmonad
ST_DIR=$DOTFILES_DIR/install/build/st

COMPUTERNAME=$(hostname -s)
PKGPROFILE=${COMPUTERNAME}
PKGLIST=$DOTFILES_DIR/install/bsdinstall/packages/$PKGPROFILE/pkglist.txt

TEST=$DOTFILES_DIR/install/bsdinstall/sanity_check.sh

sudo pkg install -y git

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
    cd ~
    mkdir -p .ssh
    chmod 700 .ssh
    ssh-keygen -t ed25519 -C "$EMAIL" -f ~/.ssh/id_ed25519
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/id_ed25519
    ;;
esac

# Install packages
echo "$PKGLIST"
read -p "Install pkglist? [y/N] " ans
case "$ans" in
    [Yy]*)
    xargs sudo pkg install -y < $PKGLIST
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
    echo "Created symlink for st"
    ;;
esac

read -p "Link dotfiles? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Link dotfiles
    $LINK_SCRIPT $LINKS
    ;;
esac

read -p "Enable system monitoring? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Add cron job for system monitoring (runs every 2 minutes)
    CRON_LINE="*/2 * * * * $SCRIPTS_DIR/battery-monitor.sh >/dev/null 2>&1; $SCRIPTS_DIR/wifi-monitor.sh >/dev/null 2>&1"
    
    # Check if cron job already exists
    if crontab -l 2>/dev/null | grep -q "battery-monitor.sh"; then
        echo "Cron job already exists"
    else
        # Add to crontab
        (crontab -l 2>/dev/null; echo "$CRON_LINE") | crontab -
        echo "Added system monitoring to crontab (runs every 2 minutes)"
    fi
    ;;
esac

read -p "Run tests? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Test
    $TEST
    ;;
esac

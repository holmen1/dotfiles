#!/bin/bash

USER=$(whoami)
EMAIL=user@gmail.com

DOTFILES_DIR=~/repos/dotfiles
SCRIPTS_DIR=$DOTFILES_DIR/scripts
LINK_SCRIPT=$DOTFILES_DIR/install/archinstall/link_x_config.sh

XMONAD_DIR=$DOTFILES_DIR/install/build/xmonad
ST_DIR=$DOTFILES_DIR/install/build/st

PKGLIST=$DOTFILES_DIR/install/archinstall/packages/suckless/pkglist.txt
FPKGLIST=$DOTFILES_DIR/install/archinstall/packages/suckless/foreignpkglist.txt

TEST=$DOTFILES_DIR/install/archinstall/sanity_check.sh

sudo pacman -S --needed git base-devel openssh

read -p "Configure git? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    # Configure git
    git config --global user.name "$USER"
    git config --global user.email "$EMAIL"
fi

read -p "Generate SSH key? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
# Generate ssh key
    cd ~
    mkdir -p .ssh
    chmod 700 .ssh
    ssh-keygen -t ed25519 -C "$EMAIL" -f ~/.ssh/id_ed25519
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/id_ed25519
fi

read -p "Install yay? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    # Install yay
    mkdir -p tmp
    cd tmp
    sudo pacman -S --needed git base-devel && git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si
    sudo -k
    cd ..
fi

# Install packages
read -p "Install pkglist? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    $SCRIPTS_DIR/yay_install.sh $PKGLIST
fi

read -p "Install foreignpkglist? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    $SCRIPTS_DIR/yay_install.sh $FPKGLIST
fi

read -p "Install Haskell? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    # Install GHCup
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    echo "Installed Haskell, may need reboot"
fi

read -p "Build xmonad? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    # Build Xmonad
    $XMONAD_DIR/build-xmonad.sh
fi

read -p "Install xmonad? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    sudo mkdir -p /opt/xmonad
    sudo cp -f $XMONAD_DIR/bin/xmonad-v0.* /opt/xmonad/
    echo "Installed xmonad to /opt/xmonad/"
fi

read -p "Build st? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    # Build st
    $ST_DIR/build-st.sh
fi

read -p "Install st? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    sudo mkdir -p /opt/st
    sudo cp -f $ST_DIR/bin/st-0.* /opt/st/
    echo "Installed st to /opt/st/"
fi

read -p "Link dotfiles? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    # Link dotfiles
    $LINK_SCRIPT
fi

read -p "Enable services? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    # Enable services
    systemctl --user daemon-reload
    systemctl --user enable system-monitor.timer
    systemctl --user start system-monitor.timer
fi

read -p "Run tests? [y/N] " ans
if [[ $ans =~ ^[Yy]$ ]]; then
    # Test
    $TEST
fi

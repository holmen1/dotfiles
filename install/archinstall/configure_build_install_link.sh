#!/bin/sh

USER=$(whoami)
EMAIL=$USER@gmail.com

DOTFILES_DIR=~/repos/dotfiles
SCRIPTS_DIR=$DOTFILES_DIR/scripts
INSTALL_SCRIPT=$SCRIPTS_DIR/pacman-install.sh
LINK_SCRIPT=$SCRIPTS_DIR/link_config.sh
LINKS=$DOTFILES_DIR/install/archinstall/links/suckless_links.config

XMONAD_DIR=$DOTFILES_DIR/install/build/xmonad
ST_DIR=$DOTFILES_DIR/install/build/st

COMPUTERNAME=$(hostnamectl --static 2>/dev/null || hostname -s)
PKGPROFILE=${COMPUTERNAME}
PKGLIST=$DOTFILES_DIR/install/archinstall/packages/$PKGPROFILE/pkglist.txt
FPKGLIST=$DOTFILES_DIR/install/archinstall/packages/$PKGPROFILE/foreignpkglist.txt

TEST=$DOTFILES_DIR/install/archinstall/sanity_check.sh

sudo pacman -S --needed openssh
sudo -k

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

read -p "Install yay? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Install yay
    mkdir -p tmp
    cd tmp
    sudo pacman -S --needed git base-devel && git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si
    sudo -k
    cd ..
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

echo "$FPKGLIST"
read -p "Install foreignpkglist? [y/N] " ans
case "$ans" in
    [Yy]*)
    $INSTALL_SCRIPT $FPKGLIST
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
    sudo -k
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
    sudo -k
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

read -p "Enable services? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Enable services
    systemctl --user daemon-reload
    systemctl --user enable --now system-monitor.timer
    ;;
esac

read -p "Run tests? [y/N] " ans
case "$ans" in
    [Yy]*)
    # Test
    $TEST
    ;;
esac

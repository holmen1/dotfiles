#!/bin/sh

USER=$(whoami)
EMAIL=$USER@gmail.com

DOTFILES_DIR=~/repos/dotfiles
INSTALL_SCRIPT=$DOTFILES_DIR/install/artixinstall/scripts/install-pacman.sh
LINK_SCRIPT=$DOTFILES_DIR/scripts/link_config.sh

COMPUTERNAME=$(hostname -s)
PKGPROFILE=${COMPUTERNAME}
PKGLIST=$DOTFILES_DIR/install/artixinstall/packages/$PKGPROFILE/pkglist.txt
FPKGLIST=$DOTFILES_DIR/install/artixinstall/packages/$PKGPROFILE/foreignpkglist.txt
LINKS=$DOTFILES_DIR/install/artixinstall/links/$PKGPROFILE/links.config

TEST=$DOTFILES_DIR/install/artixinstall/tests/$PKGPROFILE/sanity_check.sh

sudo pacman -S --needed openssh
sudo -k

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

read -p "Install yay? [y/N] " ans
case "$ans" in
    [Yy]*)
    mkdir -p tmp
    cd tmp
    sudo pacman -S --needed git base-devel && git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si
    sudo -k
    cd ..
    ;;
esac

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

read -p "Link dotfiles? [y/N] " ans
case "$ans" in
    [Yy]*)
    $LINK_SCRIPT $LINKS
    ;;
esac

read -p "Enable services? [y/N] " ans
case "$ans" in
    [Yy]*)
    # OpenRC — add services at default runlevel
    sudo rc-update add iwd default
    sudo rc-update add dbus default
    echo "Services added to default runlevel"
    ;;
esac

read -p "Run tests? [Y/n] " ans
case "$ans" in
    [Nn]) ;;
    *)    $TEST ;;
esac

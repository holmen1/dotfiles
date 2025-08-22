# dotfiles

Complete setup for Linux: configuration, installation, and build scripts

## TL;DR
```bash
git clone https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
cd ~/repos/dotfiles
```
Run [configure_build_install_link.sh](configure_build_install_link.sh)
```bash
./configure_build_install_link.sh
```

This will configure git, generate SSH keys, install yay, set up Haskell, build and install all required packages, link configuration files and executables

## Project Scope

This project describes complete setup, including building installing and dotfiles. Aiming to be minimal, stable and maintainable

## Requirements

This setup is designed to run on low-spec hardware (tested on a 4GB RAM machine) and has minimal requirements. It should work on most modern x86_64 Linux systems.

**Note:** Scripts assume Arch Linux (using pacman), but can be adapted for other distributions with minor changes.

### Included Components
- [Hyprland](https://github.com/holmen1/dotfiles/tree/master/dotfiles/hypr): a dynamic tiling Wayland compositor
- [Xmonad](https://github.com/holmen1/dotfiles/tree/master/dotfiles/xmonad): a dynamic tiling X11 window manager
- [Neovim](https://github.com/holmen1/dotfiles/tree/master/dotfiles/nvim): hyperextensible Vim-based text editor

## Installation guides
- [arch](https://github.com/holmen1/dotfiles/tree/master/install/archinstall)
- [debian](https://github.com/holmen1/dotfiles/tree/master/install/debianinstall)
- [macos](https://github.com/holmen1/dotfiles/tree/master/install/archinstall)

## Automation scripts

* Export/import packages
* Link configuration

## Build

The [build](https://github.com/holmen1/dotfiles/tree/master/install/build)
folder contains scripts and tools for building and managing components of this dotfiles setup.



 

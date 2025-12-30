# dotfiles

Minimal configuration for Linux, BSD, and macOS: dotfiles, installation guides, and build scripts

## TL;DR
```bash
git clone https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
cd ~/repos/dotfiles
```

**Arch Linux:** `./configure_build_install_link.sh`

**FreeBSD:** `./install/bsdinstall/configure_build_install_link.sh`

See [Installation guides](#installation-guides) for details.

* Window manager: Xmonad (tiling, minimal, highly configurable)
* Terminal: st (simple terminal, patched for features)
* File manager: lf (terminal-based, lightweight)
* Web browser: Brave (privacy-focused, fast)
* VPN: Mullvad CLI (secure, scriptable VPN control)
* Editor: Neovim (modern Vim, extensible)
* System monitoring: Custom scripts for battery and WiFi status

## Project Scope

Minimal, stable, and maintainable configuration across multiple operating systems. Emphasizes suckless tools and UNIX philosophy.

## Requirements

Designed for low-spec hardware (tested on 4GB RAM). Minimal dependencies.

**Supported systems:**
- Linux (Arch, Debian)
- FreeBSD
- macOS

### Included Components
- [Hyprland](https://github.com/holmen1/dotfiles/tree/master/dotfiles/hypr): a dynamic tiling Wayland compositor
- [Xmonad](https://github.com/holmen1/dotfiles/tree/master/dotfiles/xmonad): a dynamic tiling X11 window manager
- [Neovim](https://github.com/holmen1/dotfiles/tree/master/dotfiles/nvim): hyperextensible Vim-based text editor

## Installation guides
- [Arch Linux](install/archinstall)
- [Debian/Ubuntu](install/debianinstall)
- [FreeBSD](install/bsdinstall)
- [macOS](install/macinstall)

## Automation scripts

* Export/import packages
* Link configuration

## Build

The [build](https://github.com/holmen1/dotfiles/tree/master/install/build)
folder contains scripts and tools for building and managing components of this dotfiles setup.



 

# dotfiles

Minimal configuration for Linux, BSD, and macOS<sup>*</sup> : dotfiles, installation guides, and build scripts

* Display server: XLibre (X11 protocol implementation)
* Window manager: Xmonad (tiling, minimal, highly configurable)
* Editor: Neovim (Telescope, LSP, gitsigns)
* Terminal: st (simple terminal, patched for features)
* File manager: lf (terminal-based, lightweight)
* Web browser: Brave (privacy-focused, fast)
* VPN: Mullvad CLI (secure, scriptable VPN control)
* System monitoring: Custom scripts for battery and WiFi status

---
<sup>*</sup> macOS config is limited to basic functionality

## TL;DR

Clone the repository:
```bash
git clone https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
cd ~/repos/dotfiles
```
Run the appropriate script for your system (bsd or arch):

**BSD:**
```bash
$ ./install/bsdinstall/configure_build_install_link.sh
```
Then follow prompts:
```
Configure git? [y/N]
Generate SSH key? [y/N]
/home/holmen1/repos/dotfiles/install/bsdinstall/packages/besk/pkglist.txt
Install pkglist? [y/N]
Build xmonad? [y/N]
Install xmonad? [y/N]
Build st? [y/N]
Install st? [y/N]
Link dotfiles? [y/N]
Enable system monitoring? [y/N]
Run tests? [y/N]
```


See [Installation guides](#installation-guides) for details



## Project Scope

A stable, and maintainable **minimal desktop environment** configuration across multiple operating systems

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
* System monitoring

## Build

The [build](https://github.com/holmen1/dotfiles/tree/master/install/build)
folder contains scripts and tools for building and managing components (xmonad, st, xlibre) of this dotfiles setup.



 

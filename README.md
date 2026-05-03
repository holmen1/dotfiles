# dotfiles

Minimal desktop configuration for Linux, BSD, and macOS — dotfiles, installation guides, and build scripts.

## Design goals

- **Uniform structure across distros** — every install follows the same pattern: profile-based package list, stow-managed dotfiles, per-distro scripts, same prompts
- **Distro-specific via stow packages** — differences (init system, network backend, notification daemon) are handled by swapping stow packages in `links.config`, not by conditionals inside shared scripts

## Stack

* Display server: XLibre (X11)
* Window manager: Xmonad
* Menus: dmenu
* Editor: Neovim
* Terminal: st
* File manager: lf
* Dotfile linking: stow
* Keymaps: setxkbmap / XKB

---

## TL;DR

Clone the repository:
```sh
git clone https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
cd ~/repos/dotfiles
```

Run the install script for your system and follow the prompts:

```sh
# Arch
./install/archinstall/configure_build_install_link.sh

# Debian
./install/debianinstall/configure_build_install_link.sh

# FreeBSD
./install/bsdinstall/configure_build_install_link.sh
```

Prompts per step:
```
Configure git? [y/N]
Generate SSH key? [y/N]
Install yay? [y/N]
/home/holmen1/repos/dotfiles/install/archinstall/packages/x1/pkglist.txt
Install pkglist? [y/N]
/home/holmen1/repos/dotfiles/install/archinstall/packages/x1/foreignpkglist.txt
Install foreignpkglist? [y/N]
Build xmonad? [y/N]
Install xmonad? [y/N]
Build st? [y/N]
Install st? [y/N]
Build xkb keymap? [y/N]
Link dotfiles? [y/N]
Enable services? [y/N]
Run tests? [Y/n]
```

See [Installation guides](#installation-guides) for details.

## Build

The [build](install/build) folder contains scripts for building components from source: ghc, neovim, st, xkb, xmonad, xlibre.

## Project Scope

Stable, maintainable minimal desktop across multiple operating systems. Designed for low-spec hardware (tested on 4 GB RAM).

**Supported systems:**
- Linux (Arch, Debian)
- FreeBSD
- macOS (limited)

## Installation guides
- [Arch Linux](install/archinstall)
- [Debian](install/debianinstall)
- [FreeBSD](install/bsdinstall)
- [macOS](install/macinstall)

## Automation scripts

* Export/import packages per distro
* System monitoring (battery, wifi) via systemd user timers

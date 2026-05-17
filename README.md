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

- Install a base system
[Artix Wiki](https://wiki.artixlinux.org/Main/Installation)

- Clone this repository:
```sh
git clone --depth 1 --branch v0.0.4 https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
```

- Chose distro:
```sh
cd ~/repos/dotfiles/install/artixinstall
```
Run the install script for your system and follow the prompts
(current artixinstall require manual build and install of: xlibre, ghc, xmonad **NB**):

```sh
./configure_build_install_link.sh
```

Prompts per step:
```
Configure git? [y/N]
Generate SSH key? [y/N]
Install yay? [y/N]
/home/holmen1/repos/dotfiles/install/artixinstall/packages/gadsden/pkglist.txt
Install pkglist? [y/N]
/home/holmen1/repos/dotfiles/install/artixinstall/packages/gadsden/foreignpkglist.txt
Install foreignpkglist? [y/N]
Rebuild xmonad? [y/N]
Install xmonad? [y/N]
Build st? [y/N]
Install st? [y/N]
Build xkb keymap? [y/N]
Link dotfiles? [y/N]
Enable services? [y/N]
Run tests? [Y/n]
Sanity check — gadsden (artix/openrc)

--- Core commands
  [ok] git
  [ok] ssh
  [ok] xmonad
  [ok] st
  [ok] stow
  [ok] dmenu
  [ok] nvim

--- X session
  [ok] startx
  [ok] /home/holmen1/.xinitrc
  [ok] /usr/local/bin/xmonad
  [ok] xbindkeys
  [ok] scrot
  [ok] i3lock
  [ok] xterm

--- Notifications
  [ok] dunst
  [ok] notify-send

--- OpenRC services
  [ok] dbus running
  [ok] dhcpcd running
  [ok] elogind running
  [ok] iwd running

--- Dotfile symlinks
  [ok] /home/holmen1/.config/nvim -> ../repos/dotfiles/dotfiles/nvim/.config/nvim
  [ok] /home/holmen1/.xinitrc -> repos/dotfiles/dotfiles/x/.xinitrc

--- Git
  [ok] user.name: user
  [ok] user.email: user@mail.com

--- SSH
  [ok] id_ed25519 exists
  [ok] id_ed25519 perms 600

--- groups
  [ok] user in wheel group
  [ok] passwordless sudo enabled
  [ok] user in video group

Passed: 29  Failed: 0
```
---

## Structure

dotfiles/  
├── config/              # All user dotfiles (one per editor/tool)  
│   ├── nvim/  
│   ├── bash/  
│   ├── zsh/  
│   ├── x/  
│   └── ...  
├── install/             # Distro-specific installers  
│   ├── profiles/        # Profile configs (arch, debian, bsd, macos)  
│   │   ├── arch/  
│   │   ├── debian/  
│   │   ├── bsd/  
│   │   └── macos/  
│   ├── builders/        # Source-built components  
│   │   ├── xmonad/  
│   │   ├── st/  
│   │   ├── xkb/  
│   │   └── ...  
│   └── scripts/         # Shared install helpers, and utilities (monitoring, linking)  
└── README.md 




## Build

The [build](install/build) folder contains scripts for building components from source: ghc, neovim, st, xkb, xmonad, xlibre.

## Project Scope

Stable, maintainable minimal desktop across multiple operating systems. Designed for low-spec hardware (tested on 4 GB RAM).

**Supported systems:**
- Linux (Artix, Arch, Debian)
- BSD   (FreeBSD)
- macOS (limited)
- Unix  (xv6) [TODO]

## Installation guides

[Distro install guides](install)

## Automation scripts

* Export/import packages per distro
* System monitoring (battery, wifi)

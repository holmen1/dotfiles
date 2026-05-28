# dotfiles

Minimal window manager

Workstation setup for Linux, BSD, and macOS — dotfiles, installation guides, and build scripts

- Menus and monitoring handled by application launcher to replace traditional DE components
- Automation scripts for package export/import, battery monitoring, and Wi-Fi
- [Build](install/build) scripts included for source-built components:
ghc, neovim, st, xkb, xmonad, xlibre.
- Uniform [structure](#structure) across distros — every install follows the same pattern:
profile-based package list, stow-managed dotfiles, per-distro scripts
- Distro-specific via stow packages — differences (init system, network backend, notification daemon)
are handled by swapping stow packages in `links.config`, not by conditionals inside shared scripts

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

0. **Install a base system**
[Artix Wiki](https://wiki.artixlinux.org/Main/Installation)

1. **Install X server**
from your distribution's provided packages or build and install
[Xlibre](https://github.com/X11Libre/xserver/wiki/Building-XLibre) from source

2. **Run install script** that will install, link and test your system

- Clone this repository:
```sh
git clone --depth 1 --branch v0.0.4 https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
```

- Chose distro:
```sh
cd ~/repos/dotfiles/install/artixinstall
```
Run and follow the prompts
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

```bash
dotfiles/  
├── config              # All user dotfiles to stow (one per editor/tool)
│   ├── artixinstall    # Distro-specifc scripts
│   ├── bash
│   ├── brave
│   ├── chromium
│   ├── common          # Shared scripts
│   ├── lf
│   ├── nvim
│   ├── openrc
│   ├── vscode
│   ├── x
│   ├── xmonad
│   └── ...
└── install
    ├── build           # Source-built components
    │   ├── ghc
    │   ├── neovim
    │   ├── st
    │   ├── xkb
    │   ├── xlibre
    │   └── xmonad
    └── profiles        # Distro-specific installers
        ├── artixinstall
        ├── bsdinstall
        ├── debianinstall
        └── ...
```

where every distro structured uniformly
```bash
artixinstall/
├── configure_build_install_link.sh
├── links
|   └── gadsden
│       └── links.config
├── packages
|   └── gadsden
│       ├── foreignpkglist.txt
│       └── pkglist.txt
└── tests
    └── gadsden
        └── sanity_check.sh
```

## Project Scope

Stable, maintainable minimal desktop across multiple operating systems. Designed for low-spec hardware (tested on 4 GB RAM).

**Supported systems:**
- Linux (Artix, Arch, Debian)
- BSD   (FreeBSD)
- macOS (limited)
- Unix  (xv6) [TODO]

[Distro install guides](install/profiles)

## TODO

- Add `ungoogled-chromium` to the browser/profile options
- Add a post-install commands text file for Vim-based execution instead of typing
- Document install and maintenance commands in a single reference file
- Move generic post-install from distros to install
- Add description to Build/README
- Repair .md links after restructure
- Investigate if cabal flags: +with-xft should be handled by ghc

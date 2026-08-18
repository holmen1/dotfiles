# dotfiles

Minimal window manager

Workstation setup for Linux, BSD, and macOS — dotfiles, installation guides, and build scripts

Copilot instructions for models and humans: [.github/copilot-instructions.md](.github/copilot-instructions.md)

- Menus and monitoring handled by dmenu and simple shell scripts
- [Build](install/build) scripts included for source-built components:
`dwm`, `ghc`, `neovim`, `st`, `xkb`, `xmonad`, `xlibre`
- Uniform [structure](#structure) across distros — every install follows the same pattern:
profile-based package list, stow-managed dotfiles, per-distro scripts
- Distro-specific via `stow` packages — differences (init system, network backend)
are handled by swapping stow packages in
[links.config](install/profiles/artixinstall/links/x1/links.config)

## Stack

|                   | Artix         | FreeBSD   |                       
|-------------------|---------------|-----------
| Display server    | XLibre (X11)  | XLibre (X11)|
| Window manager    | Xmonad        | dwm       |
| Menus             | dmenu         | dmenu     |
| Editor / IDE      | Neovim        | Neovim    |
| Terminal          | st            | st        |
| File manager      | lf            | lf        |
| Dotfile linking   | stow          | stow      |
| Keymaps           | setxkbmap/XKB | setxkbmap/XKB|

---

## TL;DR

0. **Get and install a base system** of your choice 

|        | Artix         | FreeBSD   |                       
|--------|---------------|-----------
| .iso   | [artix-base-openrc](https://artixlinux.org/download.php)|[FreeBSD](https://freebsd.org/where)|
|My guide| [artixinstall](install/profiles/artixinstall/README.md)|[bsdinstall](install/profiles/bsdinstall/README.md)|

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
./configure-build-install-link.sh
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
│   ├── bsdinstall      # Distro-specifc scripts
│   ├── bash
│   ├── chromium
│   ├── common          # Shared scripts
│   ├── lf
│   ├── nvim
│   ├── xmonad
│   └── ...
└── install
    ├── build           # Source-built components
    │   ├── dwm
    │   ├── ghc
    │   ├── neovim
    │   ├── st
    │   ├── xkb
    │   ├── xlibre
    │   └── xmonad
    └── profiles        # Distro-specific installers
        ├── artixinstall
        ├── bsdinstall
        └── ...
```

where every distro structured uniformly
```bash
artixinstall/
├── configure-build-install-link.sh
├── links
|   └── machine01
│       └── links.config
├── packages
|   └── machine01
│       ├── foreignpkglist.txt
│       └── pkglist.txt
└── tests
    └── machine01
        └── sanity_check.sh
```

## Project Scope

Minimal "desktop" across multiple operating systems. Designed for low-spec hardware (tested on 4 GB RAM).

**Supported systems:**
- Linux: Artix (old install guides for Arch and Debian in
[stale](https://github.com/holmen1/dotfiles/tree/stale) branch, with older extras like Hyprland and xmobar kept there for reference)
- BSD: FreeBSD
- macOS: Some config (zsh, nvim, kitty)
- Unix  (xv6) [TODO]


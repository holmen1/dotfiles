# dotfiles

Minimal window manager

Workstation setup for Linux, BSD, and macOS — dotfiles, installation guides, and build scripts

Copilot instructions for models and humans: [.github/copilot-instructions.md](.github/copilot-instructions.md)
Open work index: [TODO.md](TODO.md)
Structured docs index: [docs/README.md](docs/README.md)

- Menus and monitoring handled by dmenu and simple shell scripts
- [Build](install/build) scripts included for source-built components:
`dwm`, `ghc`, `neovim`, `st`, `xkb`, `xmonad`, `xlibre`
- Uniform [structure](#structure) across distros — every install follows the same pattern:
profile-based package list, stow-managed dotfiles, and shared setup
- Distro-specific via `stow` packages — differences (init system, network backend)
are handled by swapping stow packages in
[host profiles](install/hosts/)

## Stack

|                   | Artix         | FreeBSD   |                       
|-------------------|---------------|-----------
| Display server    | Xorg / XLibre (X11) | XLibre (X11)|
| Window manager    | Xmonad        | dwm       |
| Menus             | dmenu         | dmenu     |
| Editor / IDE      | Neovim        | Neovim    |
| Terminal          | st            | st        |
| File manager      | lf            | lf        |
| Dotfile linking   | stow          | stow      |
| Keymaps           | setxkbmap/XKB | setxkbmap/XKB|

All things X11 — session startup, TTY recovery, debugging, component links:
[docs/x11.md](docs/x11.md)

---

## TL;DR

0. **Get and install a base system** of your choice 

|        | Artix         | FreeBSD   |                       
|--------|---------------|-----------
| .iso   | [artix-base-openrc](https://artixlinux.org/download.php)|[FreeBSD](https://freebsd.org/where)|
|My guide| [artixinstall](docs/artix-install.md)|[bsdinstall](docs/bsd-install.md)|

1. **Install X server**
from your distribution's provided packages or build and install
[Xlibre](https://github.com/X11Libre/xserver/wiki/Building-XLibre) from source

2. **Run install script** that will install, link and test your system

- Clone this repository:
```sh
git clone https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
```

- Run the pre-setup and general setup in order, and follow the prompts:
```sh
~/repos/dotfiles/install/pre-configure-build-install-link.sh
~/repos/dotfiles/install/configure-build-install-link.sh
```

See the [general setup guide](install/GENERAL-SETUP.md) for host profiles,
build requirements, and package export.
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
    ├── hosts           # OS and hostname-specific configuration
    ├── profiles        # Legacy FreeBSD and manual macOS setup
    ├── configure-build-install-link.sh
    └── sanity-check.sh
```

## Project Scope

Minimal "desktop" across multiple operating systems. Designed for low-spec hardware (tested on 4 GB RAM).

**Supported systems:**
- Linux: Artix (old install guides for Arch and Debian in
[stale](https://github.com/holmen1/dotfiles/tree/stale) branch, with older extras like Hyprland and xmobar kept there for reference)
- BSD: FreeBSD
- macOS: Some config (zsh, nvim, kitty)

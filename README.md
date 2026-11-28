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

1. **Clone the repository over HTTPS and run pre-setup from the TTY.** Add
   the generated SSH key to GitHub before the pre-setup's final check.

```sh
mkdir -p ~/repos
git clone https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
~/repos/dotfiles/install/scripts/pre-configure-build-install-link.sh
```

2. **Switch the remote to SSH, then continue setup.** Pre-setup offers a
   temporary xterm session for an optional X test. Full setup links the session
   configuration and builds the window manager.

```sh
git -C ~/repos/dotfiles remote set-url origin git@github.com:holmen1/dotfiles.git
git -C ~/repos/dotfiles ls-remote origin HEAD
~/repos/dotfiles/config/common/.scripts/configure-build-install-link.sh
```

See the [general setup guide](docs/guides/general-setup.md) for host profiles,
build requirements, and package export. The
[FreeBSD guide](docs/bsd-install.md) covers the optional early X test.
---

## Structure

```bash
dotfiles/  
├── config              # All user dotfiles to stow (one per editor/tool)
│   ├── artixinstall    # Artix-specific dotfiles
│   ├── bsdinstall      # FreeBSD-specific dotfiles
│   ├── bash
│   ├── chromium
│   ├── common          # Shared configuration and Stow-facing scripts
│   ├── lf
│   ├── nvim
│   ├── xmonad
│   └── ...
└── install
    ├── scripts         # Shared pre-setup and sanity scripts
    ├── build           # Source-built components
    │   ├── dwm
    │   ├── ghc
    │   ├── neovim
    │   ├── st
    │   ├── xkb
    │   ├── xlibre
    │   └── xmonad
    ├── hosts           # OS/hostname profiles and installation scripts
    │   ├── scripts     # Shared host setup helpers
    │   ├── Linux       # Artix profiles and OS-specific scripts
    │   ├── FreeBSD     # FreeBSD profiles and OS-specific scripts
    │   └── macOS       # mac02 profile and manual Homebrew setup
```

## Project Scope

Minimal "desktop" across multiple operating systems. Designed for low-spec hardware (tested on 4 GB RAM).

**Supported systems:**
- Linux: Artix (old install guides for Arch and Debian in
[stale](https://github.com/holmen1/dotfiles/tree/stale) branch, with older extras like Hyprland and xmobar kept there for reference)
- BSD: FreeBSD
- macOS: Some config (zsh, nvim, kitty)

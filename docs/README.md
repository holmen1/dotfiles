# Documentation index

Structured documentation lives here. Keep top-level READMEs short and link into
these pages for deeper explanations.

## Core guides

- [X11 guide](x11.md) — startup, TTY recovery, display manager notes, debugging.
- [Networking](networking.md) — Artix/OpenRC networking: `iwd`, `dhcpcd`, `openresolv`, service commands.
- [Service management](service-management.md) — OpenRC on Artix and the distinct FreeBSD rc system.
- [Artix installation guide](artix-install.md) — encrypted LUKS/LVM base installation.
- [Artix checklist](artix-checklist.md) — printable base and post-install checklist.
- [Simple Artix installation](artix-install-simple.md) — unencrypted UEFI install without LVM or swap.
- [Artix post-install guide](artix-post-install.md) — post-install setup, rescue notes, and package management.
- [FreeBSD installation guide](bsd-install.md) — base installation, setup, recovery, and package management.
- [Hardware notes](hardware.md) — machine-specific quirks (Framework AMD display, RDSEED, touchpad).
- [Build guide](build.md) — shared build conventions and links to component build docs.
- [Project overview](../README.md) and [installation overview](guides/install-overview.md).
- [General setup](guides/general-setup.md) — profiles and interactive setup.

## Lessons-learned handling

- Keep `LESSONS_LEARNED.md` files as incident history and forensic context.
- Move durable, repeatable rules into `docs/` guides (then link back to lessons).
- Current extracted rules:
  - XMonad runtime/recompile/deploy invariants → [docs/build.md](build.md)
  - Artix/OpenRC networking → [docs/networking.md](networking.md)
  - Artix post-install and package management → [docs/artix-post-install.md](artix-post-install.md)
  - Framework `besk` hardware notes → [docs/hardware.md](hardware.md)

## OS installation guides

- [Artix install](artix-install.md)
- [Artix simple install](artix-install-simple.md)
- [FreeBSD install](bsd-install.md)
- [macOS setup](guides/macos.md)

## Build component docs

- [Build overview](../install/build/README.md)
- [GHC](../install/build/ghc/README.md)
- [XMonad](../install/build/xmonad/README.md)
- [XLibre](../install/build/xlibre/README.md)
- [XKB](../install/build/xkb/README.md)
- [st](../install/build/st/README.md)
- [dwm](../install/build/dwm/README.md)
- [Neovim build script](../install/build/neovim/build-neovim.sh) (README not yet split out)

## Reference

- [Terminal colors](../config/colors/README.md)
- [Neovim configuration](../config/nvim/README.md) and [adding plugins](../config/nvim/ADDING_PLUGINS.md)
- [XKB configuration](../config/xkb/README.md)
- [XMonad configuration](../config/xmonad/README.md)
- [lf configuration](../config/lf/README.md)
- [Shell commands](shell.md) — reusable one-liners (mount, disk images, TTY, `cd -`, history) and `.bashrc` shortcuts.
- [Vim quick reference](vim-quickref.md) — generic Vim mappings (extracted from the nvim README).

## Archive

Superseded or out-of-scope documents, kept for history:

- [nvim-REFACTOR_PLAN.md](archive/nvim-REFACTOR_PLAN.md) — completed nvim refactor plan
- [artixinstall-README.md](archive/artixinstall-README.md) — original Artix installation README, preserved as-is
- [artixinstall-packages-README.md](archive/artixinstall-packages-README.md) — original Artix packages README, preserved as-is
- [bsdinstall-README.md](archive/bsdinstall-README.md) — original FreeBSD installation README, preserved as-is
- [bsdinstall-packages-README.md](archive/bsdinstall-packages-README.md) — original FreeBSD packages README, preserved as-is
- [legacy-bsdinstall-host-config.md](archive/legacy-bsdinstall-host-config.md) — legacy host profile instructions, superseded by the general setup
- [bsdinstall_log.txt](archive/bsdinstall_log.txt) — historical FreeBSD installation transcript
- [legacy-bsdinstall-post-install-packages.txt](archive/legacy-bsdinstall-post-install-packages.txt) — old bootstrap package list, now represented in current host profiles
- [postinstall.md](archive/postinstall.md) — former shared post-install guide, merged into OS-specific guides
- [artixinstall-LESSONS_LEARNED.md](archive/artixinstall-LESSONS_LEARNED.md) — extracted into networking, Artix post-install, and hardware docs
- [vscode-README.md](archive/vscode-README.md) — outside the stow/dotfiles system
- [ghc-toolchain-warmup.md](archive/ghc-toolchain-warmup.md) — xmonad sandbox experiment
- [potato-freebsd-i915-panic.md](archive/potato-freebsd-i915-panic.md) — X startup kernel panic and unverified workaround

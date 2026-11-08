# Documentation index

Structured documentation lives here. Keep top-level READMEs short and link into
these pages for deeper explanations.

## Core guides

- [X11 guide](x11.md) — startup, TTY recovery, display manager notes, debugging.
- [Post-install guide](postinstall.md) — WiFi, users/sudo, git/ssh, audio, docker, remote access, misc admin tasks.
- [Networking](networking.md) — Artix/OpenRC networking: `iwd`, `dhcpcd`, `openresolv`, service commands.
- [Artix install notes](artix-install.md) — Arch differences, ISO rescue path, package strategy.
- [Hardware notes](hardware.md) — machine-specific quirks (Framework AMD display, RDSEED, touchpad).
- [Build guide](build.md) — shared build conventions and links to component build docs.

## Lessons-learned handling

- Keep `LESSONS_LEARNED.md` files as incident history and forensic context.
- Move durable, repeatable rules into `docs/` guides (then link back to lessons).
- Current extracted rules:
  - XMonad runtime/recompile/deploy invariants → [docs/build.md](build.md)
  - Artix/OpenRC networking → [docs/networking.md](networking.md)
  - Artix install notes (basestrap, rescue, package strategy) → [docs/artix-install.md](artix-install.md)
  - Framework `besk` hardware notes → [docs/hardware.md](hardware.md)

## Install guides (profiles)

- [Artix install](../install/profiles/artixinstall/README.md)
- [FreeBSD install](../install/profiles/bsdinstall/README.md)
- [macOS profile](../install/profiles/macinstall/README.md)

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

- [Shell commands](shell.md) — reusable one-liners (mount, disk images, TTY, `cd -`, history) and `.bashrc` shortcuts.
- [Vim quick reference](vim-quickref.md) — generic Vim mappings (extracted from the nvim README).

## Archive

Superseded or out-of-scope documents, kept for history:

- [nvim-REFACTOR_PLAN.md](archive/nvim-REFACTOR_PLAN.md) — completed nvim refactor plan
- [artixinstall-LESSONS_LEARNED.md](archive/artixinstall-LESSONS_LEARNED.md) — extracted into networking/artix-install/hardware docs
- [vscode-README.md](archive/vscode-README.md) — outside the stow/dotfiles system
- [ghc-toolchain-warmup.md](archive/ghc-toolchain-warmup.md) — xmonad sandbox experiment

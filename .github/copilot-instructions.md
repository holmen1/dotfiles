# Copilot instructions

## Repository map

- `config/` holds the actual user configuration (dotfiles) organized by tool/component; these are symlinked into `$HOME` via stow.
- `install/hosts/` holds OS- and hostname-specific configuration plus shared and per-OS scripts; `install/hosts/macOS/mac02.conf` records the manual macOS Stow package selection and Homebrew setup.
- `install/build/` holds build scripts for source-compiled components (xmonad, dwm, st, xkb, neovim, ghc, xlibre).
- `docs/` holds structured long-form documentation (`docs/README.md` is the index); `docs/archive/` holds superseded documents kept for history.
- The stack is X11-centric. Artix uses **xmonad**; FreeBSD uses **dwm**. Both use `st` and `xkb`; X server choice is host-specific.

### Reference implementations

The general installer handles Artix and FreeBSD; the former standalone FreeBSD installer has been retired. (Old archinstall/debianinstall guides and extras like Hyprland/xmobar live on the `stale` branch; macOS remains config-only.)

## Build and install commands

### Distro installers (choose one)

- **Artix (OpenRC) and FreeBSD:** Run
  `./install/pre-configure-build-install-link.sh`, then
  `./config/common/.scripts/configure-build-install-link.sh`
- **macOS (manual):** Use `install/hosts/macOS/Brewfile` and the `install/hosts/macOS/scripts/*.sh` helpers; there is no unified installer.

### Component builds

- Build XMonad libs (Haskell dependencies): `./install/build/xmonad/build-xmonad-libs.sh`
- Build custom XMonad binary: `./install/build/xmonad/build-custom-xmonad.sh`
- Install custom XMonad: `./install/build/xmonad/install-custom-xmonad.sh`
- Build dwm: `./install/build/dwm/build-dwm.sh`
- Install dwm: `./install/build/dwm/install-dwm.sh`
- Build st (Simple Terminal): `./install/build/st/build-st.sh`
- Install st (Simple Terminal): `./install/build/st/install-st.sh`
- Build XKB keymap: `./install/build/xkb/build-xkb.sh`
- Build Neovim from source: `./install/build/neovim/build-neovim.sh`
- Install GHC from an x86_64 Debian 12 binary distribution: `./install/build/ghc/build-ghc.sh <version>` (e.g. `9.12.2`); installs to `/usr/local`
- Install XLibre: use the repo packages (`xlibre-xserver`, `xlibre-input-libinput`; on Artix via `pacman`)

## Test and verification commands

- **General sanity check:** `./config/common/.scripts/sanity-check.sh`
- **XLibre smoke test:** `./install/build/xlibre/test.sh`
- **XMonad build chain:** `build-xmonad-libs.sh` (deps), then `build-custom-xmonad.sh` (binary), then `install-custom-xmonad.sh` (deploy) — all three are required; `xmonad --recompile && xmonad --restart` alone does **not** deploy a new binary on this machine (see Architecture below).

## Architecture

- **Reproducible multi-OS setup:** The general installer supports Artix and FreeBSD; macOS is handled separately.
- **Hostname-based profiles:** `install/hosts/<OS>/<hostname>.conf` defines the package list, stow packages and components. Linux profiles additionally identify the distribution.
- **Central link configuration:** The host profile's `packages` variable selects stow packages from `config/`. The linker (`install/hosts/scripts/link_config.sh`) backs up conflicting real files as `*.bak`.
- **Standalone build factories:** Source-built components in `install/build/` are self-contained. XMonad binaries are staged to `install/build/xmonad/bin/`, then installed to `/opt/xmonad/` and symlinked at `/usr/local/bin/xmonad`. GHC is installed directly to `/usr/local`.
- **XMonad build chain:** `build-xmonad-libs.sh` uses Cabal only to install pinned `xmonad`/`xmonad-contrib` libraries into the shared Cabal store (rerun only when bumping versions). `build-custom-xmonad.sh` then compiles/links `xmonad.hs` directly with plain GHC — no per-project cabal file. Details: `install/build/xmonad/README.md`; history: `install/build/xmonad/LESSONS_LEARNED.md`.
- **`xmonad --recompile` is hijacked, and `--recompile && --restart` never deploys:** `config/xmonad/.config/xmonad/build` symlinks to `build-custom-xmonad.sh`, which writes a fresh `-rc-<timestamp>` binary under `install/build/xmonad/bin/`, never to XMonad's cache path. `--restart` only re-execs `/usr/local/bin/xmonad`, so `install-custom-xmonad.sh` must be run manually to promote a build. See `install/build/xmonad/LESSONS_LEARNED.md`.
- **Centralized color scheme:** `config/bash/.bashrc` is the single source of truth for terminal colors (`LS_COLORS`, `DMENU_*`, `OSC 10`); dmenu scripts and `lf` consume those exports. See `config/colors/README.md`.
- **X11 session setup:** `config/common/.xinitrc` reads the active host profile for the window manager, terminal, and layout; `xkb-toggle` applies the custom keymap. All X11 session/TTY/debugging topics are collected in `docs/x11.md`.

## Conventions

- **Shell dialect:** Scripts are POSIX `sh` unless explicitly marked otherwise (e.g., `#!/bin/bash`). Avoid bashisms in shared scripts.
- **Hardcoded path:** Many scripts assume the repo lives at `~/repos/dotfiles`. When editing commands or docs, keep this path in sync. If refactoring to use a different path, update all installer scripts and tests.
- **Version naming in binaries:** Compiled xmonad binaries go to `/opt/xmonad/xmonad-X.Y.Z-YYYYMMDD_HHMMSS` (release-candidate marker stripped, build timestamp kept) and are symlinked via `/usr/local/bin/xmonad` (no version in symlink). This allows parallel versions and easy rollback.
- **XKB OS variants:** The keymap is built by `build-xkb.sh` into `~/.cache/custom-keymap.xkb`. Toggle is handled by `xkb-toggle.sh` (linked to `/usr/local/bin/xkb-toggle`).
- **Distro-specific dotfiles:** If a tool differs between distros, use separate stow packages (e.g., `config/artixinstall/` and `config/bsdinstall/`). The host profile selects which package to stow.
- **Stow package organization:** Each directory in `config/` is a stow package (e.g., `config/nvim/`, `config/bash/`). The host profile specifies which packages to stow. Packages should be self-contained and not depend on each other.
- **Link config format:** The host profile is a shell snippet sourced by the linker; it sets a `packages` variable as a space-separated list of stow package names (e.g. `packages="bash nvim xkb common artixinstall lf"`). The linker stows each named directory from `config/` into `$HOME`.
- **Debugging:** When diagnosing issues, inspect the actual files and logs rather than guessing. Use sanity check scripts to verify setup state. `.xinitrc` logs missing essential programs to `~/.xinitrc-errors.log`; an invalid host profile stops startup.

## Common workflows

### Adding a new distro or computer profile

1. Copy an existing `install/hosts/<OS>/<hostname>.conf` and set the host identity, components, stow packages, and package lists.
2. Run `config/common/.scripts/configure-build-install-link.sh` and `config/common/.scripts/sanity-check.sh` on the host. A new OS or Linux distribution also requires explicit support in the general scripts.

### Modifying or adding a dotfile

1. Add or edit files under `config/<tool>/` (e.g., `config/nvim/.config/nvim/init.lua`).
2. Ensure the directory is listed in the appropriate host profile for systems that should use it.
3. If the change is tool-specific, edit only that stow package.
4. If distro-specific, create or modify the distro-specific package (e.g., `config/artixinstall/` vs `config/bsdinstall/`).
5. Re-link if the repo is already installed: `bash install/hosts/scripts/link_config.sh <profile_links_config>`.

### Documentation

- `docs/README.md` is the documentation index — register any new doc there.
- Durable, repeatable knowledge belongs in `docs/` topic guides: `x11.md` (session/TTY/debugging/input), `networking.md`, `artix-install.md`, `artix-post-install.md`, `bsd-install.md`, `bsd-post-install.md`, `hardware.md`, `build.md`, `shell.md` (reusable one-liners and the `.bashrc` shortcut index), `vim-quickref.md`.
- `LESSONS_LEARNED.md` files keep incident history and forensics; extract durable rules into `docs/` and cross-link.
- READMEs stay concise entry points that link into `docs/`; deep explanation lives in the docs pages, not in READMEs.
- Superseded documents go to `docs/archive/` with a header explaining why (never delete silently).
- After doc moves/renames, verify all relative markdown links still resolve.

### Building a component locally for testing

1. Run the build script, e.g., `./install/build/xmonad/build-xmonad-libs.sh` then `build-custom-xmonad.sh`.
2. The binary lands in `install/build/xmonad/bin/`.
3. Install with `install-custom-xmonad.sh` which copies to `/opt/xmonad/` and symlinks `/usr/local/bin/xmonad`.
4. For dwm (FreeBSD): `./install/build/dwm/build-dwm.sh` then `install-dwm.sh`.

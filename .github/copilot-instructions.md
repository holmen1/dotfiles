# Copilot instructions

## Repository map

- `config/` holds the actual user configuration (dotfiles) organized by tool/component; these are symlinked into `$HOME` via stow.
- `install/profiles/` holds OS-specific installer scripts and per-distro package lists.
- `install/build/` holds build scripts for source-compiled components (xmonad, dwm, st, xkb, neovim, ghc, xlibre).
- The stack is X11-centric. Artix uses **xmonad**; FreeBSD uses **dwm**. Both use `st`, `xkb`, and `xlibre`.

### Reference implementations

**Artixinstall and bsdinstall are the master reference implementations.** Other distros (archinstall, debianinstall, macinstall) may be partially outdated. Always consult artixinstall or bsdinstall first for patterns and approach.

## Build and install commands

### Distro installers (choose one)

- **Artix (OpenRC) — master reference:** `./install/profiles/artixinstall/configure-build-install-link.sh`
- **FreeBSD — master reference:** `./install/profiles/bsdinstall/configure-build-install-link.sh`
- **macOS (manual):** Use the `Brewfile` and the `install/profiles/macinstall/scripts/*.sh` helpers; there is no unified installer.

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
- Build/install GHC from source: `./install/build/ghc/build-ghc.sh <version>` (e.g. `9.12.2`); installs to `/usr/local`
- Install XLibre: use the repo packages (`xlibre-xserver`, `xlibre-input-libinput`; on Artix via `pacman`)

## Test and verification commands

- **Artix sanity check (master):** `./install/profiles/artixinstall/tests/<computername>/sanity_check.sh`
- **FreeBSD sanity check (master):** `./install/profiles/bsdinstall/tests/<computername>/sanity_check.sh`
- **XLibre smoke test:** `./install/build/xlibre/test.sh`
- **XMonad rebuild/debug loop:** `xmonad --recompile && xmonad --restart`
- **XMonad build chain:** `build-xmonad-libs.sh` (deps), then `build-custom-xmonad.sh` (binary), then `install-custom-xmonad.sh`

## Architecture

- **Reproducible multi-OS setup:** The repo implements the same installation pattern across Linux (Artix) and FreeBSD, with macOS handled separately — no branching logic inside shared scripts.
- **Per-distro strategy:** Each distro has its own directory (`install/profiles/<distro>install/`) containing installer scripts, package lists (pkglist.txt), link configs, and sanity check tests. The installer reads the config for that distro and executes accordingly.
- **Hostname-based profiles:** Computer name (from `hostname -s`) determines which package list and link config to use. This allows multiple machines with different setups from a single repo.
- **Central link configuration:** `install/profiles/<distro>install/links/<hostname>/links.config` defines which stow packages from `config/` to symlink into `$HOME`. The linker (`config/common/.scripts/link_config.sh`) backs up conflicting real files as `*.bak`.
- **Standalone build factories:** Source-built components in `install/build/` are self-contained. XMonad binaries are staged to `install/build/xmonad/bin/`, then installed to `/opt/xmonad/` and symlinked at `/usr/local/bin/xmonad`. GHC is installed directly to `/usr/local`.
- **X11 session setup:** Starts with `config/artixinstall/.xinitrc` (Artix) or `config/bsdinstall/.xinitrc` (FreeBSD). Artix loads XMonad; FreeBSD loads dwm. Both set a base XKB layout in `.xinitrc`, then `xkb-toggle` handles the custom compiled keymap.

## Conventions

- **Shell dialect:** Scripts are POSIX `sh` unless explicitly marked otherwise (e.g., `#!/bin/bash`). Avoid bashisms in shared scripts.
- **Hardcoded path:** Many scripts assume the repo lives at `~/repos/dotfiles`. When editing commands or docs, keep this path in sync. If refactoring to use a different path, update all installer scripts and tests.
- **Version naming in binaries:** Compiled binaries go to `/opt/<name>/xmonad-X.Y.Z` and are symlinked via `/usr/local/bin/xmonad` (no version in symlink). This allows parallel versions and easy rollback.
- **XKB OS variants:** The keymap is built by `build-xkb.sh` into `~/.cache/custom-keymap.xkb`. Toggle is handled by `xkb-toggle.sh` (linked to `/usr/local/bin/xkb-toggle`).
- **Distro differences in config, not code:** If a tool differs between distros, create separate stow packages per distro (e.g., `config/artixinstall/` and `config/bsdinstall/`) rather than adding conditionals inside shared scripts. The link config selects which package to stow.
- **Stow package organization:** Each directory in `config/` is a stow package (e.g., `config/nvim/`, `config/bash/`). The `links.config` file specifies which packages to stow. Packages should be self-contained and not depend on each other.
- **Link config format:** `links.config` is a shell snippet sourced by the linker; it sets a `packages` variable as a space-separated list of stow package names (e.g. `packages="bash nvim xkb common artixinstall lf"`). The linker stows each named directory from `config/` into `$HOME`.
- **Debugging:** When diagnosing issues, inspect the actual files and logs rather than guessing. Use sanity check scripts to verify setup state.

## Common workflows

### Adding a new distro or computer profile

1. Create `install/profiles/<distro>install/` (or `install/profiles/<distro>/` for non-install profiles).
2. Copy an existing `links/` and `packages/` directory structure, or create from scratch.
3. Under `links/<computername>/`, create `links.config` listing which stow packages to symlink.
4. Under `packages/<computername>/`, create `pkglist.txt` and optionally `foreignpkglist.txt` for non-native packages.
5. Create a test file at `tests/<computername>/sanity_check.sh` to verify the installation.
6. Copy and adapt `configure-build-install-link.sh` from artixinstall or bsdinstall.

### Modifying or adding a dotfile

1. Add or edit files under `config/<tool>/` (e.g., `config/nvim/.config/nvim/init.lua`).
2. Ensure the directory is listed in the appropriate `links.config` for systems that should use it.
3. If the change is tool-specific, edit only that stow package.
4. If distro-specific, create or modify the distro-specific package (e.g., `config/systemd-debian/` vs `config/openrc/`).
5. Re-link if the repo is already installed: `bash config/common/.scripts/link_config.sh <profile_links_config>`.

### Building a component locally for testing

1. Run the build script, e.g., `./install/build/xmonad/build-xmonad-libs.sh` then `build-custom-xmonad.sh`.
2. The binary lands in `install/build/xmonad/bin/`.
3. Install with `install-custom-xmonad.sh` which copies to `/opt/xmonad/` and symlinks `/usr/local/bin/xmonad`.
4. For dwm (FreeBSD): `./install/build/dwm/build-dwm.sh` then `install-dwm.sh`.

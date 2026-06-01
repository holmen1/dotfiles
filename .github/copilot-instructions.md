# Copilot instructions

## Repository map

- `config/` holds the actual user configuration (dotfiles) organized by tool/component; these are symlinked into `$HOME` via stow.
- `install/profiles/` holds OS-specific installer scripts and per-distro package lists.
- `install/build/` holds build scripts for source-compiled components (xmonad, st, xkb, neovim, ghc, xlibre).
- The stack is X11/XMonad-centric: `xmonad`, `st`, `xkb`, and `xlibre` are the main custom pieces.

### Reference implementations

**Artixinstall and bsdinstall are the master reference implementations.** Other distros (archinstall, debianinstall, macinstall) may be partially outdated. Always consult artixinstall or bsdinstall first for patterns and approach.

## Build and install commands

### Distro installers (choose one)

- **Artix (OpenRC) — master reference:** `./install/profiles/artixinstall/configure_build_install_link.sh`
- **FreeBSD — master reference:** `./install/profiles/bsdinstall/configure_build_install_link.sh`
- **Arch (may be outdated):** `./install/profiles/archinstall/configure_build_install_link.sh`
- **Debian (may be outdated):** `./install/profiles/debianinstall/configure_build_install_link.sh`
- **macOS (may be outdated):** Manual steps using `brew bundle` and the `links/` config. See `install/profiles/macinstall/README.md` for current guidance.

### Component builds

- Build XMonad: `./install/build/xmonad/build-xmonad.sh`
- Rebuild XMonad (after config changes): `./install/build/xmonad/rebuild-xmonad.sh`
- Build st (Simple Terminal): `./install/build/st/build-st.sh`
- Build XKB keymap: `./install/build/xkb/build-xkb.sh`
- Build Neovim from source: `./install/build/neovim/build-neovim.sh`
- Build/install GHC from source: `cd install/build/ghc && ./configure && sudo make install`
- Install XLibre (Arch/AUR): `yay -S xlibre-xserver-bootstrap`, `yay -S xlibre-input-libinput`, then `yay -S xlibre-xserver`

## Test and verification commands

- **Artix sanity check (master):** `./install/profiles/artixinstall/tests/<computername>/sanity_check.sh`
- **FreeBSD sanity check (master):** `./install/profiles/bsdinstall/tests/<computername>/sanity_check.sh`
- **XLibre smoke test:** `./install/build/xlibre/test.sh`
- **XMonad rebuild/debug loop:** `xmonad --recompile && xmonad --restart`

## Architecture

- **Reproducible multi-OS setup:** The repo implements the same installation pattern across Linux (Artix, Arch, Debian), FreeBSD, and macOS—no branching logic inside shared scripts.
- **Per-distro strategy:** Each distro has its own directory (`install/profiles/<distro>install/`) containing installer scripts, package lists (pkglist.txt, foreignpkglist.txt), link configs, and sanity check tests. The installer reads the config for that distro and executes accordingly.
- **Hostname-based profiles:** Computer name (from `hostname -s`) determines which package list and link config to use. This allows multiple machines with different setups from a single repo.
- **Central link configuration:** `config/<distro>/<hostname>/links.config` defines which stow packages from `config/` to symlink into `$HOME`. The linker (`config/common/.scripts/link_config.sh`) backs up conflicting real files as `*.bak`.
- **Standalone build factories:** Source-built components in `install/build/` are self-contained; each emits versioned binaries to `/opt/<name>/` and is symlinked into `/usr/local/bin/`.
- **X11 session setup:** Starts with `config/x/.xinitrc`, then loads XMonad config from `config/xmonad/` and XKB keymap from `config/xkb/`. Multiple keymaps by OS: `keymap-linux.xkb`, `keymap-bsd.xkb`, etc.

## Conventions

- **Shell dialect:** Scripts are POSIX `sh` unless explicitly marked otherwise (e.g., `#!/bin/bash`). Avoid bashisms in shared scripts.
- **Hardcoded path:** Many scripts assume the repo lives at `~/repos/dotfiles`. When editing commands or docs, keep this path in sync. If refactoring to use a different path, update all installer scripts and tests.
- **Version naming in binaries:** Compiled binaries go to `/opt/<name>/xmonad-X.Y.Z` and are symlinked via `/usr/local/bin/xmonad` (no version in symlink). This allows parallel versions and easy rollback.
- **XKB OS variants:** The keymap build outputs different files per OS: `keymap-linux.xkb`, `keymap-bsd.xkb`. The `.xinitrc` or `xmonad-session-rc` selects the right one at runtime.
- **Distro differences in config, not code:** If a tool differs between distros (e.g., OpenRC vs systemd), create separate stow packages (e.g., `config/systemd-debian/` and `config/openrc/`) rather than adding conditionals inside shared scripts. The link config selects which package to stow.
- **Stow package organization:** Each directory in `config/` is a stow package (e.g., `config/nvim/`, `config/bash/`). The `links.config` file specifies which packages to stow. Packages should be self-contained and not depend on each other.
- **Link config format:** `links.config` is a simple text file; each line is either a comment (`#`) or a package name. The linker stows matching directories from `config/` into `$HOME`.
- **Debugging:** When diagnosing issues, inspect the actual files and logs rather than guessing. Use sanity check scripts to verify setup state.

## Common workflows

### Adding a new distro or computer profile

1. Create `install/profiles/<distro>install/` (or `install/profiles/<distro>/` for non-install profiles).
2. Copy an existing `links/` and `packages/` directory structure, or create from scratch.
3. Under `links/<computername>/`, create `links.config` listing which stow packages to symlink.
4. Under `packages/<computername>/`, create `pkglist.txt` and optionally `foreignpkglist.txt` for non-native packages.
5. Create a test file at `tests/<computername>/sanity_check.sh` to verify the installation.
6. Copy and adapt `configure_build_install_link.sh` from artixinstall or bsdinstall.

### Modifying or adding a dotfile

1. Add or edit files under `config/<tool>/` (e.g., `config/nvim/.config/nvim/init.lua`).
2. Ensure the directory is listed in the appropriate `links.config` for systems that should use it.
3. If the change is tool-specific, edit only that stow package.
4. If distro-specific, create or modify the distro-specific package (e.g., `config/systemd-debian/` vs `config/openrc/`).
5. Re-link if the repo is already installed: `bash config/common/.scripts/link_config.sh <profile_links_config>`.

### Building a component locally for testing

1. Run the build script, e.g., `./install/build/xmonad/build-xmonad.sh`.
2. The binary lands in `install/build/xmonad/bin/`.
3. For XMonad, also run sanity checks: `xmonad --recompile && xmonad --restart`.
4. After testing, the component build script typically installs it to `/opt/` and symlinks it.

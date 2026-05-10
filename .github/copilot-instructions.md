# Copilot instructions

## Repository map

- `dotfiles/` holds the actual user configuration that gets linked into `$HOME`.
- `install/` holds OS-specific install guides plus build factories for source-built tools.
- `scripts/` holds reusable shell helpers for linking configs, package export/import, and system utilities.
- The stack is X11/XMonad-centric: `xmonad`, `st`, `xkb`, and `xlibre` are the main custom pieces.

## Build and install commands

- Arch bootstrap/install flow: `./install/archinstall/configure_build_install_link.sh`
- Debian bootstrap/install flow: `./install/debianinstall/configure_build_install_link.sh`
- FreeBSD bootstrap/install flow: `./install/bsdinstall/configure_build_install_link.sh`
- macOS package export/import and linking: `brew bundle dump --file=~/repos/dotfiles/install/macinstall/Brewfile --force`, `brew bundle --file=~/repos/dotfiles/install/macinstall/Brewfile`, then `./scripts/link_config.sh install/macinstall/macos_links.conf`

- Build XMonad: `./install/build/xmonad/build-xmonad.sh`
- Build st: `./install/build/st/build-st.sh`
- Build XKB keymap: `./install/build/xkb/build-xkb.sh`
- Build Neovim from source: `./install/build/neovim/build-neovim.sh`
- Build/install GHC from source: `cd install/build/ghc && ./configure && sudo make install`
- Install XLibre on Arch/AUR: `yay -S xlibre-xserver-bootstrap`, `yay -S xlibre-input-libinput`, then `yay -S xlibre-xserver`

## Test and verification commands

- FreeBSD installation sanity check: `./install/bsdinstall/sanity_check.sh`
- XLibre smoke test: `./install/build/xlibre/test.sh`
- XMonad rebuild/debug loop: `xmonad --recompile && xmonad --restart`

## Architecture

- The repo is organized around reproducible desktop setup across Linux, FreeBSD, and macOS.
- Per-distro setup scripts choose the right package lists, link configs, and post-install steps instead of branching inside shared helpers.
- `scripts/link_config.sh` is the central dotfile linker; it reads a distro config, stows the selected packages into `$HOME`, and backs up conflicting real files as `*.bak`.
- Source-built components live under `install/build/` and are treated as standalone build factories that emit versioned binaries or generated config artifacts into the dotfiles tree.
- X11 session startup is split between `dotfiles/x/.xinitrc` and XMonad/XKB configuration under `dotfiles/xmonad/` and `dotfiles/xkb/`.

## Conventions

- Shell scripts are POSIX `sh` unless a script explicitly says otherwise.
- Many scripts assume the repo lives at `~/repos/dotfiles`; keep that path in sync when editing commands or docs.
- Versioned binaries are installed under `/opt/<name>/` and then symlinked into `/usr/local/bin/`.
- XKB output is generated into `dotfiles/xkb/.config/xkb/` and differs by OS (`keymap-linux.xkb` vs `keymap-bsd.xkb`).
- Keep distro-specific differences in the per-distro link/package config files rather than adding conditionals to shared scripts.
- Do not guess when diagnosing logs or behavior; if evidence is incomplete, say so and inspect the relevant file, script, or log before concluding.

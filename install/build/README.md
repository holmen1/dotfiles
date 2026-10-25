# Build

Scripts and tools for building source-based components in this setup.

For shared build conventions and generic patterns, see
[docs/build.md](../../docs/build.md).

## Components

- [ghc](./ghc/README.md) — Glasgow Haskell Compiler from source
- [neovim](./neovim/build-neovim.sh) — Neovim source build script (README not yet split out)
- [st](./st/README.md) — suckless terminal build/install
- [xkb](./xkb/README.md) — custom keymap build pipeline
- [xlibre](./xlibre/README.md) — display server install/troubleshooting
- [xmonad](./xmonad/README.md) — XMonad build factory and deploy
- [dwm](./dwm/README.md) — dwm build/install

## Quick start

Use each component’s README for exact commands. Typical pattern:

1. Install prerequisites
2. Run `build-*.sh`
3. Run `install-*.sh` (if present)
4. Run relevant sanity checks

For a central summary of methods (`configure/make`, CMake, Cargo, uninstall
patterns), see [docs/build.md](../../docs/build.md).

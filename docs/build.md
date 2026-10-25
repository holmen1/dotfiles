# Build guide

Shared build conventions for this repo. Component-specific commands stay in
`install/build/<component>/README.md`.

## Build model

- Source builds are organized under `install/build/`.
- Component scripts are the canonical entry points.
- For XMonad, deployment is explicit: build libs, build binary, then install
  (`install-custom-xmonad.sh`) to promote to `/usr/local/bin/xmonad`.

## XMonad operational invariants (canonical)

These are the durable rules extracted from `install/build/xmonad/LESSONS_LEARNED.md`.

- `xmonad --recompile` runs the configured build flow, but does **not** deploy to
  `/usr/local/bin/xmonad`.
- `xmonad --restart` re-execs `xmonad` from `$PATH`; it does **not** install a new
  binary for you.
- For this repo, the deploy path is always:
  `build-xmonad-libs.sh` → `build-custom-xmonad.sh` → `install-custom-xmonad.sh`.
- If behavior looks stale after rebuild+install, inspect:
  - `~/.cache/xmonad/` (compiled cache artifacts)
  - `~/.local/share/xmonad/xmonad.state` (restored runtime state)
  - current running binary via `/proc/<pid>/exe` (or equivalent process inspection).

For full incident timelines and deep forensic notes, keep using:
[`install/build/xmonad/LESSONS_LEARNED.md`](../install/build/xmonad/LESSONS_LEARNED.md).

## Common workflow

1. Read the component README under `install/build/<component>/`.
2. Install prerequisites for that component.
3. Run the component build script.
4. Run the install script (if provided).
5. Run component sanity/smoke checks.

## Get source (generic pattern)

```sh
# Git
git clone <repo> && cd <repo>

# Tarball
curl -L -o package-version.tar.gz https://example.com/package-version.tar.gz
sha256sum package-version.tar.gz
tar -xzf package-version.tar.gz
cd package-version/
```

## Common build methods

```sh
# Autotools
./configure && make && sudo make install

# CMake
cmake -B build && cmake --build build && sudo cmake --install build

# Haskell (Setup.lhs style)
runhaskell Setup.lhs configure --user
runhaskell Setup.lhs build
runhaskell Setup.lhs install
```

## Uninstall patterns

```sh
sudo make uninstall
# or
sudo xargs rm < install_manifest.txt
```

Haskell package cleanup:

```sh
ghc-pkg unregister package-name
rm -rf ~/.cabal/lib/*package-name*
```

## Rust tools with Cargo

```sh
curl https://sh.rustup.rs -sSf | sh
source "$HOME/.cargo/env"
cargo install --locked <crate>
cargo install --list
```

## Component references

| Component | README |
|---|---|
| GHC | [`install/build/ghc/README.md`](../install/build/ghc/README.md) |
| XMonad | [`install/build/xmonad/README.md`](../install/build/xmonad/README.md) |
| XLibre | [`install/build/xlibre/README.md`](../install/build/xlibre/README.md) |
| XKB | [`install/build/xkb/README.md`](../install/build/xkb/README.md) |
| st | [`install/build/st/README.md`](../install/build/st/README.md) |
| dwm | [`install/build/dwm/README.md`](../install/build/dwm/README.md) |
| Neovim | [`install/build/neovim/build-neovim.sh`](../install/build/neovim/build-neovim.sh) |

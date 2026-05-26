# Build
Scripts and tools for building and managing components of this dotfiles setup

[ghc](https://github.com/holmen1/dotfiles/tree/master/install/build/ghc)
Glasgow Haskell Compiler (GHC) - built from source

[neovim](https://github.com/holmen1/dotfiles/tree/master/install/build/neovim)
Neovim 0.12 - built from source on FreeBSD (not yet available via `pkg` or ports as of March 2026)

[st](https://github.com/holmen1/dotfiles/tree/master/install/build/st)
Simple Terminal (st) from suckless.org - a minimal terminal emulator

[xkb](https://github.com/holmen1/dotfiles/tree/master/install/build/xkb)
Custom XKB keymap for X11 — Swedish layout

[xlibre](https://github.com/holmen1/dotfiles/tree/master/install/build/xlibre)
Fork of X.Org that must be built from AUR on Arch (available via `pkg` on FreeBSD)

[xmonad](https://github.com/holmen1/dotfiles/tree/master/install/build/xmonad)
A lightweight tiling window manager for X11 written in Haskell

## Get Source
```bash
# From git repository
git clone <repo> && cd <repo>

# OR download tarball
curl -L -o package-version.tar.gz https://example.com/package-version.tar.gz

# Verify checksum (recommended)
sha256sum package-version.tar.gz

# Extract
tar -xzf package-version.tar.gz
cd package-version/
```

## Build Methods
```bash
# Autotools (configure/make)
./configure && make && sudo make install

# CMake
cmake -B build && cmake --build build && sudo cmake --install build

# Haskell (runhaskell)
runhaskell Setup.lhs configure --user
runhaskell Setup.lhs build
runhaskell Setup.lhs install
```

## Uninstall
```bash
# Autotools/cmake (if supported)
sudo make uninstall  # from build directory
# OR
sudo xargs rm < install_manifest.txt  # if generated

# Haskell packages
ghc-pkg unregister package-name
rm -rf ~/.cabal/lib/*package-name*

# Manual cleanup (check install location)
sudo rm /usr/local/bin/program
sudo rm -rf /usr/local/share/program
```
```

## Install with Cargo
For Rust tools (e.g., `tree-sitter-cli`, `ripgrep`):
```bash
# Install Rust toolchain if missing
curl https://sh.rustup.rs -sSf | sh
source "$HOME/.cargo/env"

cargo install <crate>               # latest version
cargo install --locked <crate>      # reproducible, recommended for CLI tools
```
Binaries land in `~/.cargo/bin` — ensure it's on your `PATH`:
```bash
export PATH="$HOME/.cargo/bin:$PATH"
```

Manage installed crates:
```bash
cargo install --list                # show installed crates and versions
cargo install <crate>               # upgrade to latest
cargo uninstall <crate>             # remove
```



# XMonad Build Factory

XMonad is always built from source — no cabal-install required. The build script
`build-xmonad-ghc.sh` fetches all dependencies directly from Hackage and builds
with plain GHC using `runhaskell Setup.hs`.

## Build scripts

| Script | Purpose |
|---|---|
| `build-xmonad-ghc.sh` | Build using GHC only (no cabal-install). Preferred. |
| `build-xmonad.sh` | Legacy build using cabal-install. |

---

## Prerequisites

### System C libraries
```bash
# Arch/Artix
pacman -S libx11 libxrandr libxext libxinerama libxss pkgconf autoconf

# Debian/Ubuntu
apt install libx11-dev libxft-dev libxinerama-dev libxrandr-dev libxss-dev pkg-config autoconf
```

### GHC

Install GHC 9.8.4 via the build factory:
```bash
./install/build/ghc/build-ghc.sh 9.8.4
```
See [GHC build README](../ghc/README.md) for details.

---

## Build

```bash
./install/build/xmonad/build-xmonad-ghc.sh
```

The script:
1. Verifies GHC 9.8.4 is installed at `~/.local/ghc-9.8.4/`
2. Fetches all Haskell dependencies from Hackage
3. Builds xmonad and xmonad-contrib
4. Compiles a custom binary from `dotfiles/xmonad/xmonad.hs`
5. Places the versioned binary in `bin/` and runs a health check

---

## Deploy

```bash
sudo mkdir -p /opt/xmonad
sudo cp bin/xmonad-0.18.1 /opt/xmonad/
sudo ln -sf /opt/xmonad/xmonad-0.18.1 /usr/local/bin/xmonad
```

Target machines only need X11 runtime libraries, not Haskell:
```bash
# Arch/Artix
pacman -S libx11 libxft libxinerama libxrandr libxss xterm
```

**Note:** target machines cannot recompile without rebuilding the binary on the build machine.


This trade-off of flexibility for size and simplicity is the core of the "build factory" approach.

---

See [LESSONS_LEARNED.md](LESSONS_LEARNED.md) for lessons learned.

---
# Examining XMonad Binaries to Understand Size Differences


## Basic Analysis

```bash
# Compare file types
file ~/.local/bin/xmonad
file ~/.cache/xmonad/xmonad-x86_64-linux

# See what libraries they depend on
ldd ~/.local/bin/xmonad
ldd ~/.cache/xmonad/xmonad-x86_64-linux

# Check section sizes
size ~/.local/bin/xmonad
size ~/.cache/xmonad/xmonad-x86_64-linux
```

## Looking at Debug Symbols

```bash
# Count symbols in each binary
nm ~/.local/bin/xmonad | wc -l
nm ~/.cache/xmonad/xmonad-x86_64-linux | wc -l

# Create a stripped copy to see impact of debug symbols
cp ~/.cache/xmonad/xmonad-x86_64-linux /tmp/xmonad-stripped
strip /tmp/xmonad-stripped
ls -la /tmp/xmonad-stripped
```

## Deeper Analysis

```bash
# Examine section headers
readelf -S ~/.local/bin/xmonad | grep -A2 "\[.*\] \."
readelf -S ~/.cache/xmonad/xmonad-x86_64-linux | grep -A2 "\[.*\] \."

# Check compilation flags (might show optimization level)
readelf -p .comment ~/.local/bin/xmonad
readelf -p .comment ~/.cache/xmonad/xmonad-x86_64-linux
```

These commands will help you understand:
1. Whether debug symbols are present (explaining larger size)
2. Which optimization levels were used 
3. Whether static vs dynamic linking differs
4. Which sections contribute to size differences

The cache binary is likely larger because it contains debug information to help with error reporting during development, whereas the installed binary may be optimized for size and performance.

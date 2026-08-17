# XMonad Build Factory

XMonad is always built from source — no cabal-install required. The build script
`build-xmonad.sh` fetches all dependencies directly from Hackage and builds
with plain GHC using `runhaskell Setup.hs`.

## Build scripts

| Script                    | Purpose |
|---------------------------|---------|
| `build-xmonad.sh`         | Build libraries using GHC (no cabal-install). Preferred |
| `rebuild-xmonad.sh`       | Compile and link customZZ xmonad |
| `install-xmonad.sh`       | Install custom xmonad |
| `build-xmonad-cabal.sh`   | Legacy build using cabal-install. |

---

## Prerequisites

### System C libraries
```bash
# Arch/Artix
pacman -S libx11 libxrandr libxext libxinerama libxss pkgconf autoconf
```

### GHC

Ensure GHC used tested for current version.
If there is no tested version in your package manager,
[build GHC from source](../ghc/README.md).

---

## Build libraries and custom xmonad

```bash
./build-xmonad.sh
```
1. Verifies GHC is installed
2. Fetches all Haskell dependencies from Hackage
3. Builds xmonad and xmonad-contrib
4. Writes progress to logfile

```bash
./rebuild-xmonad.sh
```
1. Compiles a custom binary from `dotfiles/config/xmonad/xmonad.hs`
2. Places the versioned binary in `bin/` and runs a health check

---

## Install

```bash
./install-xmonad.sh
```
1. Copies (safe) to `/opt/xmonad/`
2. Links it to `/usr/local/bin/xmonad`


Target machines only need X11 runtime libraries, not Haskell:
```bash
# Arch/Artix
pacman -S libx11 [libxft?] libxinerama libxrandr libxss xterm
```

**Note:** target machines cannot recompile without rebuilding the binary on the build machine.


This trade-off of flexibility for size and simplicity is the core of the "build factory" approach.

---

See [LESSONS_LEARNED.md](LESSONS_LEARNED.md) for lessons learned.


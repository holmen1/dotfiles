# XMonad Build Factory

XMonad is always built from source — no cabal-install required. The build script
`build-xmonad-libs.sh` fetches all dependencies directly from Hackage and builds
with plain GHC using `runhaskell Setup.hs`.

## Build scripts

| Script                    | Purpose |
|---------------------------|---------|
| `build-xmonad-libs.sh`         | Build libraries using GHC (no cabal-install). Preferred |
| `build-custom-xmonad.sh`       | Compile and link custom xmonad |
| `install-custom-xmonad.sh`       | Install custom xmonad |
| `legacy-build-xmonad-cabal.sh`   | Legacy build using cabal-install. |

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
./build-xmonad-libs.sh
```
1. Verifies GHC is installed
2. Fetches all Haskell dependencies from Hackage
3. Builds xmonad and xmonad-contrib
4. Writes progress to logfile

```bash
./build-custom-xmonad.sh
```
1. Compiles a custom binary from `dotfiles/config/xmonad/xmonad.hs`
2. Places the versioned binary in `bin/` and runs a health check

---

## Install

```bash
./install-custom-xmonad.sh
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

## TODO

-[] Cabal build custom xmonad


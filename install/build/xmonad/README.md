# XMonad Build Factory

Cabal builds and caches the libraries (`xmonad`, `xmonad-contrib`), fetched
directly from Hackage. Plain GHC then compiles and links `xmonad.hs` against
them - no generated `.cabal` project needed.

## Build scripts

| Script                     | Purpose |
|----------------------------|---------|
| `build-xmonad-libs.sh`     | Install pinned xmonad/xmonad-contrib libraries via Cabal |
| `build-custom-xmonad.sh`   | Compile and link custom xmonad with GHC |
| `install-custom-xmonad.sh` | Install custom xmonad |

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

### Test cabal toolcain

Run `sandbox/smoke-test.sh` to test a simple build

## Install

```bash
./install-custom-xmonad.sh
```
Selects the newest release candidate from `bin/`, copies it to
`/opt/xmonad/xmonad-X.Y.Z-YYYYMMDD_HHMMSS` (without the `-rc-` marker), and
symlinks `/usr/local/bin/xmonad` to that installed binary. Older installed
versions remain in `/opt/xmonad/` for manual rollback.


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

-[x] Cabal build custom xmonad
-[] xmonad --recompile && --restart
-[] Configure LSP

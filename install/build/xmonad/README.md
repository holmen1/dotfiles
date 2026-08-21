# XMonad Build Factory

The build script use Cabal and fetches all dependencies directly from Hackage

## Build scripts

| Script                     | Purpose |
|----------------------------|---------|
| `build-custom-xmonad.sh`   | Compile and link custom xmonad |
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
Copies (safe) to `~/.cabal/bin/xmonad`, ensure on `$PATH`


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
-[] xmonad --recompile
-[] Configure LSP


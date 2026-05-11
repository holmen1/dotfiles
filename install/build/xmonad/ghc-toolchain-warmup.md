# GHC Toolchain Warmup for XMonad Build

## Overview

This document outlines a warmup exercise to confirm our Haskell toolchain works for building packages from Hackage using only GHC (no cabal-install). This is preparation for compiling xmonad without cabal.

## Prerequisites

- GHC installed (verify with `ghc --version`)
- curl for downloading tarballs
- Basic Unix tools (tar, mkdir, etc.)
- System C libraries if building packages that need them

## Analogy to GCC/C Development

If you're familiar with building C libraries from source (like with GCC), this Haskell process is very similar:

**C Development Flow:**
1. Download source tarball: `wget/curl https://example.com/libfoo-1.0.tar.gz`
2. Extract: `tar xzf libfoo-1.0.tar.gz`
3. Configure: `./configure --prefix=/usr/local`
4. Build: `make`
5. Install: `sudo make install`

**Haskell Development Flow (no cabal-install):**
1. Download package tarball: `curl -L -o package.tar.gz https://hackage.haskell.org/package/package-version/package-version.tar.gz`
2. Extract: `tar xzf package.tar.gz`
3. Configure: `runhaskell Setup.lhs configure --user`
4. Build: `runhaskell Setup.lhs build`
5. Install: `runhaskell Setup.lhs install`

The key difference is that Haskell packages use `runhaskell Setup.lhs` instead of `./configure && make && make install`. Both are manual build processes that bypass package managers (apt/yum for C, cabal-install for Haskell).

## Test Plan

We'll build a minimal Haskell package from Hackage to verify:
1. Package fetching works
2. `runhaskell Setup.lhs` commands execute successfully (note: some packages use .lhs instead of .hs)
3. Compilation completes without cabal-install

### Selected Test Package

We'll use `data-default-class` version 0.1.2.2 - a simple, dependency-free package that's also used in the xmonad build script.

## Steps

### 1. Verify GHC Installation

```bash
ghc --version
```

Expected output: Something like "The Glorious Glasgow Haskell Compilation System, version 9.12.2"

### 2. Set Up Build Environment

```bash
BUILD_DIR=~/repos/dotfiles/install/build/xmonad
WORK_DIR=$BUILD_DIR/_ghc_test
mkdir -p "$WORK_DIR"
cd "$WORK_DIR"
```

### 3. Fetch Package from Hackage

```bash
HACKAGE="https://hackage.haskell.org/package"
PKG="data-default-class"
VER="0.1.2.2"
TARBALL="${PKG}-${VER}.tar.gz"
URL="${HACKAGE}/${PKG}-${VER}/${TARBALL}"

echo "Fetching $PKG-$VER..."
curl -L -o "$TARBALL" "$URL"
tar xzf "$TARBALL"
cd "${PKG}-${VER}"
```

### 4. Build with runhaskell Setup.lhs

Some Haskell packages, especially older ones, use `Setup.lhs` (literate Haskell) instead of `Setup.hs`. The `runhaskell` command can execute both file types, but you must use the correct filename as found in the package directory.

```bash
runhaskell Setup.lhs configure --user
runhaskell Setup.lhs build
runhaskell Setup.lhs install
```

### 6. Confirm Package Usability and Linking

Create a test program that defines a custom instance of the `Default` type class to verify linking works:

```haskell
import Data.Default.Class

data MyType = MyType Int String deriving Show

instance Default MyType where
    def = MyType 42 "default"

main :: IO ()
main = do
    let myDef = def :: MyType
    putStrLn $ "Default MyType: " ++ show myDef
    putStrLn "data-default-class linking successful!"
```

Compile and run:

```bash
ghc --make test-default.hs -o test-default
./test-default
```

Expected output:
```
Default MyType: MyType 42 "default"
data-default-class linking successful!
```

## Package Installation Locations

When using `--user` with `runhaskell Setup.lhs`, packages are installed to your user directories (not system-wide):

- **Compiled libraries**: `~/.cabal/lib/x86_64-linux-ghc-X.Y.Z-.../`
- **Package registration**: `~/.ghc/x86_64-linux-X.Y.Z/package.conf.d/`

This is the same infrastructure used by cabal-install, but we're bypassing the package manager and building manually. You can verify installation with:

```bash
ghc-pkg list --user  # Shows user-installed packages
```

## Results

✅ **Toolchain Warmup Successful!**

- GHC 9.12.2 is installed and working
- Package fetching from Hackage works
- `runhaskell Setup.lhs` commands execute successfully
- Package built and installed without cabal-install
- `data-default-class-0.1.2.2` appears in `ghc-pkg list`
- Package can be imported, linked, and the `Default` type class can be used

## Next Steps

The toolchain is confirmed ready. Proceed to the full xmonad build using `build-xmonad-ghc.sh`.

## Troubleshooting

- If `runhaskell Setup.hs` fails with "Can't find Setup.hs": Some packages use `Setup.lhs` instead. Check the package directory with `ls Setup.*` and use the correct filename.
- If `runhaskell` fails: Check GHC installation
- If configure fails: Missing system dependencies (rare for this package)
- If network issues: Check Hackage URL accessibility
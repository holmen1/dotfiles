#!/bin/sh
#
# Build xmonad + xmonad-contrib using only GHC (no cabal-install).
# Uses runhaskell Setup.hs with GHC's built-in Cabal library.
# Target: GHC 9.8.4 (base-4.19)
#
# Prerequisites:
#   - GHC, check that version is tested
#   - System C libraries: libX11, libXrandr, libXext, libXinerama, libXScrnSaver
#     Artix/Arch: pacman -S libx11 libxrandr libxext libxinerama libxss
#   - autoconf (for X11 Haskell package)

set -e

if command -v ghc >/dev/null 2>&1; then
    echo "Using GHC from PATH: $(command -v ghc)"
else
    echo "Error: no ghc found on PATH"
    exit 1
fi

if ! command -v runhaskell >/dev/null 2>&1; then
    echo "Error: runhaskell not found (needed to run Setup.hs)"
    exit 1
fi

XMONAD_VER="0.18.1"
XMONAD_CONTRIB_VER="0.18.2"

HACKAGE="https://hackage.haskell.org/package"

BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BIN_DIR=$BUILD_DIR/bin
WORK_DIR=$BUILD_DIR/_ghc_build

# Haskell packages to fetch from Hackage (non-boot dependencies)
DATA_DEFAULT_CLASS_VER="0.1.2.2"
SETLOCALE_VER="1.0.0.10"
SPLITMIX_VER="0.1.3.2"
RANDOM_VER="1.2.1.2"
UTF8_STRING_VER="1.0.2"
X11_VER="1.10.3"

mkdir -p "$WORK_DIR"

# ── helpers ──────────────────────────────────────────────────────────

fetch_hackage() {
    pkg=$1; ver=$2
    tarball="${pkg}-${ver}.tar.gz"
    url="${HACKAGE}/${pkg}-${ver}/${tarball}"
    if [ ! -f "$WORK_DIR/$tarball" ]; then
        echo "Fetching $pkg-$ver ..."
        curl -L -o "$WORK_DIR/$tarball" "$url"
    fi
    cd "$WORK_DIR"
    rm -rf "${pkg}-${ver}"
    tar xzf "$tarball"
}

build_simple() {
    pkg=$1; ver=$2
    echo "── Building $pkg-$ver ──"
    fetch_hackage "$pkg" "$ver"
    cd "$WORK_DIR/${pkg}-${ver}"
    if [ -f Setup.lhs ]; then
        SETUP_FILE=Setup.lhs
    else
        SETUP_FILE=Setup.hs
    fi
    runhaskell "$SETUP_FILE" configure --user
    runhaskell "$SETUP_FILE" build
    runhaskell "$SETUP_FILE" install
    echo "── Installed $pkg-$ver ──"
}

# ── build non-boot dependencies in order ─────────────────────────────

build_simple "data-default-class" "$DATA_DEFAULT_CLASS_VER"

# NB: setlocale-1.0.0.10 tarball has stale base <4.16; Hackage revised it to <4.23
# but the tarball is unchanged — patching .cabal in-place until a new release ships
fetch_hackage "setlocale" "$SETLOCALE_VER"
sed -i 's/base >=4.6 && <4.16/base >= 4.6/' "$WORK_DIR/setlocale-$SETLOCALE_VER/setlocale.cabal"
cd "$WORK_DIR/setlocale-$SETLOCALE_VER"
SETUP_FILE=Setup.hs
runhaskell "$SETUP_FILE" configure --user
runhaskell "$SETUP_FILE" build
runhaskell "$SETUP_FILE" install
echo "── Installed setlocale-$SETLOCALE_VER ──"

build_simple "splitmix"            "$SPLITMIX_VER"
build_simple "random"             "$RANDOM_VER"
build_simple "utf8-string"        "$UTF8_STRING_VER"
build_simple "X11"             "$X11_VER"
build_simple "xmonad"         "$XMONAD_VER"
build_simple "xmonad-contrib" "$XMONAD_CONTRIB_VER"

echo "Building packages complete."


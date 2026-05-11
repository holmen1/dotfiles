#!/bin/sh
#
# Build xmonad + xmonad-contrib using only GHC (no cabal-install).
# Uses runhaskell Setup.hs with GHC's built-in Cabal library.
#
# Prerequisites:
#   - GHC installed (e.g. from install/build/ghc/)
#   - System C libraries: libX11, libXrandr, libXext, libXinerama, libXScrnSaver
#     Artix/Arch: pacman -S libx11 libxrandr libxext libxinerama libxss
#   - autoconf (for X11 Haskell package)

set -e

XMONAD_TAG="v0.18.1"
XMONAD_CONTRIB_TAG="v0.18.2"

HACKAGE="https://hackage.haskell.org/package"

BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BIN_DIR=$BUILD_DIR/bin
WORK_DIR=$BUILD_DIR/_ghc_build
CONFIG_SOURCE=~/repos/dotfiles/dotfiles/xmonad/xmonad.hs

# Haskell packages to fetch from Hackage (non-boot dependencies)
DATA_DEFAULT_CLASS_VER="0.1.2.2"
SETLOCALE_VER="1.0.0.10"
SPLITMIX_VER="0.1.0.2"
RANDOM_VER="1.2.1.2"
UTF8_STRING_VER="1.0.2"
X11_VER="1.10.3"

mkdir -p "$BIN_DIR" "$WORK_DIR"

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
    runhaskell Setup.hs configure --user
    runhaskell Setup.hs build
    runhaskell Setup.hs install
    echo "── Installed $pkg-$ver ──"
}

build_configure() {
    pkg=$1; ver=$2
    echo "── Building $pkg-$ver (configure) ──"
    fetch_hackage "$pkg" "$ver"
    cd "$WORK_DIR/${pkg}-${ver}"
    runhaskell Setup.hs configure --user
    runhaskell Setup.hs build
    runhaskell Setup.hs install
    echo "── Installed $pkg-$ver ──"
}

# ── build non-boot dependencies in order ─────────────────────────────

build_simple "data-default-class" "$DATA_DEFAULT_CLASS_VER"
build_simple "setlocale"          "$SETLOCALE_VER"
build_simple "splitmix"           "$SPLITMIX_VER"
build_simple "random"             "$RANDOM_VER"
build_simple "utf8-string"        "$UTF8_STRING_VER"
build_configure "X11"             "$X11_VER"

# ── build xmonad from git ────────────────────────────────────────────

echo "── Building xmonad $XMONAD_TAG ──"
cd "$WORK_DIR"
rm -rf xmonad
git clone --depth 1 --branch "$XMONAD_TAG" https://github.com/xmonad/xmonad.git
cd xmonad
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install
echo "── Installed xmonad $XMONAD_TAG ──"

echo "── Building xmonad-contrib $XMONAD_CONTRIB_TAG ──"
cd "$WORK_DIR"
rm -rf xmonad-contrib
git clone --depth 1 --branch "$XMONAD_CONTRIB_TAG" https://github.com/xmonad/xmonad-contrib.git
cd xmonad-contrib
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install
echo "── Installed xmonad-contrib $XMONAD_CONTRIB_TAG ──"

# ── compile custom xmonad binary ─────────────────────────────────────

echo "── Compiling custom xmonad binary ──"
mkdir -p "$WORK_DIR/custom"
ln -sf "$CONFIG_SOURCE" "$WORK_DIR/custom/xmonad.hs"
cd "$WORK_DIR/custom"

ghc --make xmonad.hs \
    -package xmonad \
    -package xmonad-contrib \
    -package X11 \
    -o xmonad-custom

cp xmonad-custom "$BIN_DIR/xmonad-$XMONAD_TAG"
chmod +x "$BIN_DIR/xmonad-$XMONAD_TAG"

# archive
cd "$BIN_DIR"
tar -czf "xmonad-$XMONAD_TAG.tar.gz" "xmonad-$XMONAD_TAG"

echo ""
echo "Build complete."
echo "Binary:  $BIN_DIR/xmonad-$XMONAD_TAG"
echo "Archive: $BIN_DIR/xmonad-$XMONAD_TAG.tar.gz"
echo ""
echo "Install with:"
echo "  sudo mkdir -p /opt/xmonad"
echo "  sudo cp $BIN_DIR/xmonad-$XMONAD_TAG /opt/xmonad/"
echo "  sudo ln -sf /opt/xmonad/xmonad-$XMONAD_TAG /usr/local/bin/xmonad"

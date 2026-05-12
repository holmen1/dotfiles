#!/bin/sh
#
# Build xmonad + xmonad-contrib using only GHC (no cabal-install).
# Uses runhaskell Setup.hs with GHC's built-in Cabal library.
# Target: GHC 9.8.4 (base-4.19)
#
# Prerequisites:
#   - GHC 9.8.4 installed (e.g. from install/build/ghc/build-ghc.sh 9.8.4)
#   - System C libraries: libX11, libXrandr, libXext, libXinerama, libXScrnSaver
#     Artix/Arch: pacman -S libx11 libxrandr libxext libxinerama libxss
#   - autoconf (for X11 Haskell package)

set -e

GHC_VERSION="9.8.4"
GHC_BIN="${HOME}/.local/ghc-${GHC_VERSION}/bin"

if [ ! -x "${GHC_BIN}/ghc" ]; then
    echo "Error: GHC ${GHC_VERSION} not found at ${GHC_BIN}"
    echo "Run: install/build/ghc/build-ghc.sh ${GHC_VERSION}"
    exit 1
fi

export PATH="${GHC_BIN}:${PATH}"
echo "Using $(ghc --version)"

XMONAD_VER="0.18.1"
XMONAD_CONTRIB_VER="0.18.2"

HACKAGE="https://hackage.haskell.org/package"

BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BIN_DIR=$BUILD_DIR/bin
WORK_DIR=$BUILD_DIR/_ghc_build
CONFIG_SOURCE=~/repos/dotfiles/dotfiles/xmonad/xmonad.hs

# Haskell packages to fetch from Hackage (non-boot dependencies)
DATA_DEFAULT_CLASS_VER="0.1.2.2"
SETLOCALE_VER="1.0.0.10"
SPLITMIX_VER="0.1.3.2"
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

build_configure() {
    pkg=$1; ver=$2
    echo "── Building $pkg-$ver (configure) ──"
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
build_configure "X11"             "$X11_VER"

build_simple "xmonad"         "$XMONAD_VER"
build_simple "xmonad-contrib" "$XMONAD_CONTRIB_VER"

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

cp xmonad-custom "$BIN_DIR/xmonad-$XMONAD_VER"
chmod +x "$BIN_DIR/xmonad-$XMONAD_VER"

# archive
cd "$BIN_DIR"
tar -czf "xmonad-$XMONAD_VER.tar.gz" "xmonad-$XMONAD_VER"

echo ""
echo "Build complete."
echo "Binary:  $BIN_DIR/xmonad-$XMONAD_VER"
echo "Archive: $BIN_DIR/xmonad-$XMONAD_VER.tar.gz"

# Health check
BINARY="$BIN_DIR/xmonad-$XMONAD_VER"
if "$BINARY" --version 2>/dev/null | grep -q "xmonad"; then
    echo "Health check: OK ($($BINARY --version))"
else
    echo "Health check: FAIL — binary did not respond to --version"
fi

echo ""
echo "Install with:"
echo "  sudo mkdir -p /opt/xmonad"
echo "  sudo cp $BIN_DIR/xmonad-$XMONAD_VER /opt/xmonad/"
echo "  sudo ln -sf /opt/xmonad/xmonad-$XMONAD_VER /usr/local/bin/xmonad"

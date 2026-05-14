#!/bin/sh
#
# Rebuild xmonad + xmonad-contrib using only GHC (no cabal-install).
# Uses runhaskell Setup.hs with GHC's built-in Cabal library.
# Target: GHC 9.8.4 (base-4.19)

set -e

GHC_VERSION="9.12.2"
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

BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BIN_DIR=$BUILD_DIR/bin
WORK_DIR=$BUILD_DIR/_ghc_build
CONFIG_SOURCE=~/repos/dotfiles/dotfiles/xmonad/xmonad.hs

mkdir -p "$BIN_DIR" "$WORK_DIR"

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

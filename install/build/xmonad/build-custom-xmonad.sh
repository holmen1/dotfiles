#!/bin/sh
#
# Compile and link custom xmonad using only GHC (no cabal-install).

set -e

XMONAD_VER="0.18.1"
CONFIG_SOURCE=~/repos/dotfiles/config/xmonad/xmonad.hs

if command -v ghc >/dev/null 2>&1; then
    echo "Using GHC from PATH: $(command -v ghc)"
else
    echo "Error: no ghc found on PATH"
    exit 1
fi

BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BIN_DIR=$BUILD_DIR/bin
WORK_DIR=$BUILD_DIR/_ghc_build

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

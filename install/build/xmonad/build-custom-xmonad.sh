#!/bin/sh
#
# Compile and link the custom xmonad binary using plain GHC, against the
# libraries/environment file installed by build-xmonad-libs.sh.
#
# No cabal project is needed here - GHC auto-discovers the GHC environment
# file (.ghc.environment.*) when invoked with cwd set to the same directory
# it lives in, which is why we cd into $ENV_DIR (= ~/.config/xmonad, where
# xmonad.hs already lives via stow) before compiling.

set -e

XMONAD_VER="0.18.1"
ENV_DIR=~/.config/xmonad
BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BIN_DIR="$BUILD_DIR/bin"

if command -v ghc >/dev/null 2>&1; then
    echo "Using GHC from PATH: $(command -v ghc)"
else
    echo "Error: no ghc found on PATH"
    exit 1
fi

if [ ! -f "$ENV_DIR/xmonad.hs" ]; then
    echo "Error: $ENV_DIR/xmonad.hs not found - stow the xmonad package first"
    exit 1
fi

mkdir -p "$BIN_DIR"

echo ""
echo "=== Compiling custom xmonad binary ==="
cd "$ENV_DIR"
ghc --make xmonad.hs \
    -fforce-recomp \
    -main-is main \
    -o "$BIN_DIR/xmonad-$XMONAD_VER"

# Clean up GHC's intermediate build artifacts left next to the config source
rm -f "$ENV_DIR"/*.o "$ENV_DIR"/*.hi

echo ""
echo "Binary: $BIN_DIR/xmonad-$XMONAD_VER"

# Health check
BINARY="$BIN_DIR/xmonad-$XMONAD_VER"
if "$BINARY" --version 2>/dev/null | grep -q "xmonad"; then
    echo "Health check: OK ($($BINARY --version))"
else
    echo "Health check: FAIL — binary did not respond to --version"
fi

#!/bin/sh
#
# Compile and link the custom xmonad binary using plain GHC, against the
# libraries/environment file installed by build-xmonad-libs.sh.
#
# No cabal project is needed here - GHC auto-discovers the GHC environment
# file (.ghc.environment.*) when invoked with cwd set to the same directory
# it lives in.

set -e

XMONAD_VER="0.18.1"
ENV_DIR=~/.config/xmonad
BUILD_DIR=~/repos/dotfiles/install/build/xmonad
WORK_DIR="$BUILD_DIR/work"
BIN_DIR="$BUILD_DIR/bin"
RELEASE_CANDIDATE=$BIN_DIR/xmonad-$XMONAD_VER-rc-"$(date +%Y%m%d_%H%M%S)"

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
cd "$WORK_DIR"
ghc --make $ENV_DIR/xmonad.hs \
    -Wall \
    -fforce-recomp \
    -main-is main \
    -outputdir "$WORK_DIR" \
    -o "$RELEASE_CANDIDATE"

echo ""
echo "Binary: $RELEASE_CANDIDATE"

# Health check
if "$RELEASE_CANDIDATE" --version 2>/dev/null | grep -q "xmonad"; then
    echo "Health check: OK ($("$RELEASE_CANDIDATE" --version))"
else
    echo "Health check: FAIL — binary did not respond to --version"
fi

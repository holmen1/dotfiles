#!/bin/sh
#
# Compile and link the custom xmonad binary using plain GHC, against the
# libraries/environment file installed by build-xmonad-libs.sh.
#
# No cabal project is needed here, neither .ghc.environment.*,
# since explicit library links gcc-style

set -e

. "$HOME/repos/dotfiles/install/build/xmonad/versions.sh"
ENV_DIR=~/.config/xmonad
BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BIN_DIR="$BUILD_DIR/bin"
RELEASE_CANDIDATE=$BIN_DIR/xmonad-$XMONAD_VER-rc-"$(date +%Y%m%d_%H%M%S)"

check_ghc_version

if [ ! -f "$ENV_DIR/xmonad.hs" ]; then
    echo "Error: $ENV_DIR/xmonad.hs not found - stow the xmonad package first"
    exit 1
fi

mkdir -p "$BIN_DIR"

echo ""
echo "=== Compiling custom xmonad binary ==="
"$GHC_PATH" --make "$ENV_DIR/xmonad.hs" \
    -Wall \
    -clear-package-db \
    -global-package-db \
    -package-db "${HOME}"/.cabal/store/ghc-"$GHC_VER"/package.db \
    -package "xmonad-$XMONAD_VER" \
    -package "xmonad-contrib-$XMONAD_CONTRIB_VER" \
    -fforce-recomp \
    -outputdir "$BIN_DIR" \
    -o "$RELEASE_CANDIDATE"

echo ""
echo "Binary: $RELEASE_CANDIDATE"

# Health check
if "$RELEASE_CANDIDATE" --version 2>/dev/null | grep -q "xmonad"; then
    echo "Health check: OK ($("$RELEASE_CANDIDATE" --version))"
else
    echo "Health check: FAIL — binary did not respond to --version"
fi

#!/bin/sh
#
# Install the custom xmonad binary, following the repo convention:
# versioned binaries live in /opt/<name>/, symlinked (unversioned) from
# /usr/local/bin/ - this keeps parallel versions around for easy rollback,
# and decouples the running window manager from this repo's checkout.

set -e

XMONAD_VER="0.18.1"
BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BINARY="$BUILD_DIR/bin/xmonad-$XMONAD_VER"
OPT_DIR="/opt/xmonad"

if [ ! -f "$BINARY" ]; then
    echo "Error: $BINARY not found - run build-custom-xmonad.sh first"
    exit 1
fi

echo "Installing xmonad-$XMONAD_VER to $OPT_DIR"
sudo mkdir -p "$OPT_DIR"
sudo cp "$BINARY" "$OPT_DIR/xmonad-$XMONAD_VER"
sudo ln -sf "$OPT_DIR/xmonad-$XMONAD_VER" /usr/local/bin/xmonad

echo ""
echo "Installed: $OPT_DIR/xmonad-$XMONAD_VER"
echo "Linked:    /usr/local/bin/xmonad -> $OPT_DIR/xmonad-$XMONAD_VER"

# Health check
if command -v xmonad >/dev/null && xmonad --version 2>/dev/null | grep -q "xmonad"; then
    echo "Health check: OK ($(xmonad --version))"
else
    echo "Health check: FAIL — /usr/local/bin/xmonad did not respond to --version"
fi

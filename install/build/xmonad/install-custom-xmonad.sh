#!/bin/sh
#
# Install the custom xmonad binary, following the repo convention:
# versioned binaries live in /opt/<name>/, symlinked (unversioned) from
# /usr/local/bin/ - this keeps parallel versions around for easy rollback,
# and decouples the running window manager from this repo's checkout.

set -e

XMONAD_VER="0.18.1"
BUILD_DIR=~/repos/dotfiles/install/build/xmonad
OPT_DIR="/opt/xmonad"

BINARY=$(find "$BUILD_DIR/bin" -type f \
    -name "xmonad-$XMONAD_VER-rc-*" -print | sort -r | sed -n '1p')

if [ -z "$BINARY" ]; then
    echo "Error: no xmonad release candidates found in $BUILD_DIR/bin"
    exit 1
fi

INSTALLED_NAME=$(basename "$BINARY" | sed 's/-rc-/-/')
INSTALLED_BINARY="$OPT_DIR/$INSTALLED_NAME"

echo "Installing $(basename "$BINARY") as $INSTALLED_BINARY"
sudo mkdir -p "$OPT_DIR"
sudo cp "$BINARY" "$INSTALLED_BINARY"
sudo ln -sf "$INSTALLED_BINARY" /usr/local/bin/xmonad

echo ""
echo "Installed: $INSTALLED_BINARY"
echo "Linked:    /usr/local/bin/xmonad -> $INSTALLED_BINARY"

# Health check
if "$INSTALLED_BINARY" --version 2>/dev/null | grep -q "xmonad"; then
    echo "Health check: OK ($("$INSTALLED_BINARY" --version))"
else
    echo "Health check: FAIL — $INSTALLED_BINARY did not respond to --version"
fi

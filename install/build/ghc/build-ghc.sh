#!/bin/sh
set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <ghc-version>"
    echo "  e.g. $0 9.12.2"
    exit 1
fi

# Configuration
GHC_VERSION="$1"
GHC_ARCH="x86_64-deb12-linux"
GHC_DIR="${HOME}/repos/dotfiles/install/build/ghc"

# Fetch source if not already present
SRC_DIR=$(find "$GHC_DIR" -maxdepth 1 -type d -name "ghc-${GHC_VERSION}-*" 2>/dev/null | head -1)
if [ -z "$SRC_DIR" ]; then
    TARBALL="ghc-${GHC_VERSION}-${GHC_ARCH}.tar.xz"
    URL="https://downloads.haskell.org/~ghc/${GHC_VERSION}/${TARBALL}"
    echo "Downloading GHC ${GHC_VERSION}..."
    cd "$GHC_DIR"
    rm -f "$TARBALL"
    curl -L -O "$URL"
    tar -xJf "$TARBALL"
    rm "$TARBALL"
    SRC_DIR=$(find "$GHC_DIR" -maxdepth 1 -type d -name "ghc-${GHC_VERSION}-*")
fi

echo "Installing GHC ${GHC_VERSION} to /usr/local..."
cd "$SRC_DIR"

./configure
sudo make install
sudo -k

echo ""
ghc --version
which ghc

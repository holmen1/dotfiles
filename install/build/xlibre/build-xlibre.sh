#!/bin/sh
set -e

export XLIBRE_SRC="$(pwd)"
export XLIBRE_BUILD="${XLIBRE_SRC}/build"
export XLIBRE_PREFIX="${XLIBRE_SRC}/image"

# Set version and xserver variables
export VERSION="25.1.3"
export XSERVER="xserver-${VERSION}.tar.gz"

# Check for --get flag
if [[ "$1" == "--get" ]]; then
    curl -L -o "$XSERVER" "https://github.com/X11Libre/xserver/archive/refs/tags/xlibre-xserver-${VERSION}.tar.gz"
    mkdir -p "$XLIBRE_SRC/xserver"
    if ! tar -xzf "$XSERVER" -C "$XLIBRE_SRC/xserver" --strip-components=1; then
        echo "Extraction failed. Check if the tarball is valid."
        exit 1
    fi
    exit 0
fi

if [[ ! -d "$XLIBRE_SRC/xserver" ]]; then
    echo "Source directory not found. Run with --get first."
    exit 1
fi

cd "$XLIBRE_SRC/xserver"
meson setup --prefix "$XLIBRE_PREFIX" "$XLIBRE_BUILD"
ninja -C "$XLIBRE_BUILD"
sudo ninja -C "$XLIBRE_BUILD" install

# Check for --test flag
if [[ "$1" == "--test" ]]; then
    echo "Running X server test (10 seconds)..."
    "$XLIBRE_PREFIX/bin/X" :1 vt8 &
    _pid=$!
    sleep 10 && kill $_pid
    wait $_pid 2>/dev/null || true
    echo "Test complete."
fi
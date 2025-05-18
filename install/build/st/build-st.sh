#!/bin/bash
set -e

# Configuration
ST_VERSION="0.9.2"
BUILD_DIR="${HOME}/repos/dotfiles/install/build/st"
INSTALL_DIR="${HOME}/.local/bin"
PATCH_DIR="${BUILD_DIR}/patches"

echo "=== Building st ${ST_VERSION} ==="

# Create build directory
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Clean up any previous build
rm -f "st-${ST_VERSION}.tar.gz"
rm -rf "st-${ST_VERSION}"

# Download source
echo "Downloading st ${ST_VERSION}..."
curl -L "https://dl.suckless.org/st/st-${ST_VERSION}.tar.gz" | tar xz

# Enter source directory
cd "st-${ST_VERSION}"

# Apply patches
echo "Applying patches..."
for patch in "${PATCH_DIR}"/*.diff; do
    if [ -f "$patch" ]; then
        echo "Applying patch: $(basename "$patch")"
        patch -p1 < "$patch" || echo "Warning: Patch $(basename "$patch") failed, continuing..."
    fi
done

# Apply custom configuration
echo "Applying custom config.h..."
cp -f "${BUILD_DIR}/config.h" .

# Build and install
echo "Building st..."
make clean
make

# Create install directory and copy binary
mkdir -p "${INSTALL_DIR}"
cp st "${INSTALL_DIR}/"

echo "st ${ST_VERSION} has been built and installed to ${INSTALL_DIR}/st"
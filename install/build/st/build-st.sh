#!/bin/bash
set -e

# Configuration
ST_VERSION="0.9.2"
PATCH_ALPHA_VERSION="20220206-0.8.5"
PATCH_SCROLLBACK_VERSION="0.9.2"
BUILD_DIR="${HOME}/repos/dotfiles/install/build/st"
INSTALL_DIR="${HOME}/.local/bin"
BUILD_DATE=$(date +"%Y%m%d")
BUILD_TAG="st-${ST_VERSION}-${BUILD_DATE}"

echo "=== Building st ${ST_VERSION} ==="

# Create build directory
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Clean up any previous build
rm -rf "st-${ST_VERSION}"

# Download source
echo "Downloading st ${ST_VERSION}..."
curl -L "https://dl.suckless.org/st/st-${ST_VERSION}.tar.gz" | tar xz

# Enter source directory
cd "st-${ST_VERSION}"

# Download and apply patches
echo "Downloading and applying patches..."

# Alpha (transparency) patch
echo "Applying alpha patch..."
curl -O "https://st.suckless.org/patches/alpha/st-alpha-${PATCH_ALPHA_VERSION}.diff"
patch -p1 < "st-alpha-${PATCH_ALPHA_VERSION}.diff" || echo "Warning: Alpha patch failed"

# Scrollback patch
echo "Applying scrollback patch..."
curl -O "https://st.suckless.org/patches/scrollback/st-scrollback-${PATCH_SCROLLBACK_VERSION}.diff"
patch -p1 < "st-scrollback-${PATCH_SCROLLBACK_VERSION}.diff" || echo "Warning: Scrollback patch failed"

# Create config.h from patched config.def.h
echo "Using patched default configuration..."
cp -f config.def.h config.h

# Build and install
echo "Building st..."
make clean
make

# Create install directory and copy binary
mkdir -p "${INSTALL_DIR}"
cp st "${INSTALL_DIR}/${BUILD_TAG}"
chmod +x "${INSTALL_DIR}/${BUILD_TAG}"

# Create a symlink to the latest build
ln -sf "${INSTALL_DIR}/${BUILD_TAG}" "${INSTALL_DIR}/st"

echo "st ${ST_VERSION} has been built and installed to ${INSTALL_DIR}/st"
echo "Tagged as: ${BUILD_TAG}"
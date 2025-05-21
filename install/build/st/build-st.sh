#!/bin/bash
set -e

# Configuration
ST_VERSION="0.9.2"
BUILD_DIR="${HOME}/repos/dotfiles/install/build/st"
PATCH_DIR="${BUILD_DIR}/patches"
INSTALL_DIR="${HOME}/.local/bin"
BUILD_DATE=$(date +"%Y%m%d")
BUILD_TAG="st-${ST_VERSION}-${BUILD_DATE}"

PATCH_TYPES=(
  "scrollback"
  "font2"
  "alpha"
)

echo "=== Building st ${ST_VERSION} ==="

# Clean up any previous build
rm -rf "st-${ST_VERSION}"

# Download source
echo "Downloading st ${ST_VERSION}..."
curl -L "https://dl.suckless.org/st/st-${ST_VERSION}.tar.gz" | tar xz

# Enter source directory
cd "st-${ST_VERSION}"

echo "Applying patches..."
for type in "${PATCH_TYPES[@]}"; do
  patch_file=$(find "${PATCH_DIR}" -name "st-${type}*.diff" -type f)
  
  echo "Applying ${type} patch: $(basename "$patch_file")"
  patch -p1 < "$patch_file" || echo "Warning: Patch $(basename "$patch_file") failed, continuing..."
done

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

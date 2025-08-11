#!/bin/bash
set -e

# Configuration
ST_VERSION="0.9.2"
BUILD_DIR="${HOME}/repos/dotfiles/install/build/st"
PATCH_DIR="${BUILD_DIR}/patches"
BIN_DIR="$BUILD_DIR/bin"

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
  patch_file=$(find "${PATCH_DIR}" -name "st-${type}*.diff" -type f | head -n1)
  
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

# Copy binary to BIN_DIR for manual handling
echo "Installing to $BIN_DIR"
mkdir -p $BIN_DIR
cp st "$BIN_DIR/st-$ST_VERSION"
chmod +x "$BIN_DIR/st-$ST_VERSION"

# Create compressed binary archive
echo "Creating compressed binary archive..."
cd "$BIN_DIR"
tar -czf "st-$ST_VERSION.tar.gz" "st-$ST_VERSION"

echo "Build complete - st binary ready for manual deployment"
echo "Binary: $BIN_DIR/st-$ST_VERSION"
echo "Archive: $BIN_DIR/st-$ST_VERSION.tar.gz"

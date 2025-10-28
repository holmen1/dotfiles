#!/bin/bash
set -e

# Configuration
ST_VERSION="0.9.3"
ST_DIR="${HOME}/repos/dotfiles/install/build/st"
BUILD_DIR="${ST_DIR}/st-patched"
BIN_DIR="$ST_DIR/bin"


# Enter source directory
cd "${BUILD_DIR}"

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

echo "Binary: $BIN_DIR/st-$ST_VERSION"
echo "Archive: $BIN_DIR/st-$ST_VERSION.tar.gz"

# Clean up build artifacts in source directory
cd "$BUILD_DIR"
make clean

echo "Build complete - st binary ready for manual deployment"

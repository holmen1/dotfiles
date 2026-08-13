#!/bin/sh
set -e

# Configuration
ST_VERSION="0.9.3"
ST_DIR="${HOME}/repos/dotfiles/install/build/st"
BUILD_DIR="${ST_DIR}/st-patched"
BIN_DIR="$ST_DIR/bin"

# Enter source directory
cd "${BUILD_DIR}"

# Build and local install
echo "Building st..."
make clean install

# Create compressed binary archive
echo "Creating compressed binary archive..."
cd "$BIN_DIR"
cp st "st-$ST_VERSION"
tar -czf "st-$ST_VERSION.tar.gz" "st-$ST_VERSION"
echo "Binary: $BIN_DIR/st-$ST_VERSION"
echo "Archive: $BIN_DIR/st-$ST_VERSION.tar.gz"

# Clean up build artifacts in source directory
cd "$BUILD_DIR"
make clean

echo "Build complete - st binary ready for manual deployment"


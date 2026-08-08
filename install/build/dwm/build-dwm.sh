#!/bin/sh
set -e

# Configuration
DWM_VERSION="6.8"
DWM_DIR="${HOME}/repos/dotfiles/install/build/dwm"
BUILD_DIR="${DWM_DIR}/dwm-patched"
BIN_DIR="$DWM_DIR/bin"


# Enter source directory
cd "${BUILD_DIR}"

# Build and install
echo "Building dwm..."
make clean install

# Create compressed binary archive
echo "Creating compressed binary archive..."
cd "$BIN_DIR"
cp dwm "dwm-$DWM_VERSION"
tar -czf "dwm-$DWM_VERSION.tar.gz" "dwm-$DWM_VERSION"
echo "Binary: $BIN_DIR/dwm-$DWM_VERSION"
echo "Archive: $BIN_DIR/dwm-$DWM_VERSION.tar.gz"

# Clean up build artifacts in source directory
cd "$BUILD_DIR"
make clean

echo "Build complete - dwm binary ready for manual deployment"

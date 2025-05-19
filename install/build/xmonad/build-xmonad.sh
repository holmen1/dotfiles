#!/bin/bash

set -e  # Exit on error

XMONAD_TAG="v0.18.0"
XMONAD_CONTRIB_TAG="v0.18.1" 

# Define directories
BUILD_DIR=~/repos/dotfiles/install/build/xmonad
INSTALL_DIR=$HOME/.local/bin
CONFIG_SOURCE=~/repos/dotfiles/dotfiles/xmonad/xmonad.hs

# Create the build directory if it doesn't exist
mkdir -p $BUILD_DIR
cd $BUILD_DIR

# Function to clone or update a repository
clone_repo() {
  local repo_name=$1
  local repo_url=$2
  local tag=$3
  
  # Remove existing repo directory if it exists
  rm -rf "$BUILD_DIR/$repo_name"
  
  echo "Cloning $repo_name repository at $tag..."
  git clone "$repo_url" "$BUILD_DIR/$repo_name"
  cd "$BUILD_DIR/$repo_name"
  git checkout $tag
  cd "$BUILD_DIR"
}

# Always clone fresh xmonad and xmonad-contrib repositories
clone_repo "xmonad" "https://github.com/xmonad/xmonad" "$XMONAD_TAG"
clone_repo "xmonad-contrib" "https://github.com/xmonad/xmonad-contrib" "$XMONAD_CONTRIB_TAG"

# Copy your configuration to build directory
echo "Using custom configuration from $CONFIG_SOURCE"
mkdir -p "$BUILD_DIR/custom-xmonad"
ln -sf "$CONFIG_SOURCE" "$BUILD_DIR/custom-xmonad/xmonad.hs"

# Build custom XMonad
echo "Building custom XMonad binary..."
cd "$BUILD_DIR/custom-xmonad"

# Create a simple cabal file that includes your configuration
cat > custom-xmonad.cabal << EOF
cabal-version:      2.4
name:               custom-xmonad
version:            0.1.0.0
build-type:         Simple

executable xmonad
  main-is:          xmonad.hs
  build-depends:    base, xmonad, xmonad-contrib
  default-language: Haskell2010
EOF

# Initialize cabal project
echo "package xmonad
  flags: +with-xft

package xmonad-contrib
  flags: +with-xft" > cabal.project.local

# Create a proper project structure that Cabal can recognize
echo "packages: ." > cabal.project

# Build the custom binary
echo "Running cabal update..."
cabal update
echo "Building with cabal..."
cabal build

# Install the binary
echo "Installing to $INSTALL_DIR"
find dist-newstyle -name xmonad -type f -executable -exec cp {} "$INSTALL_DIR/xmonad" \;
chmod +x "$INSTALL_DIR/xmonad"

echo "Build complete - your configuration is now baked into the XMonad binary"

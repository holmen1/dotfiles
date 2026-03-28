#!/bin/sh
set -e

NVIM_TAG="nightly"
NV_DIR="${HOME}/repos/dotfiles/install/build/neovim/neovim"

git clone https://github.com/neovim/neovim $NV_DIR
cd $NV_DIR
git checkout $NVIM_TAG
# gmake required on FreeBSD (BSD make is incompatible); on Linux/macOS use make
gmake CMAKE_BUILD_TYPE=Release
sudo gmake install

echo "Done: $(nvim --version | head -1)"

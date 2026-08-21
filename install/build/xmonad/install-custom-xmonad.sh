#!/bin/sh
set -e
BIN_DIR=$HOME/.cabal/bin

echo "Backup current, install rc" 
cp -a "$BIN_DIR"/xmonad "$BIN_DIR"/xmonad.bak && \
mv "$BIN_DIR"/xmonad-rc "$BIN_DIR"/xmonad && \
echo "success!" || \
echo "	!! mv xmonad-rc xmonad failed" 


#!/bin/sh
set -e

# Configuration
DWM_DIR="${HOME}/repos/dotfiles/install/build/dwm"
BIN_DIR="$DWM_DIR/bin"


sudo mkdir -p /opt/dwm
sudo rm -f /opt/dwm/*
sudo cp -f $BIN_DIR/dwm-6.[0-9] /opt/dwm/
sudo ln -sf /opt/dwm/dwm-6.* /usr/local/bin/dwm
sudo -k
echo "Created symlink for dwm"

#!/bin/sh
set -e

# Configuration
ST_DIR="${HOME}/repos/dotfiles/install/build/st"
BIN_DIR="$ST_DIR/bin"


sudo mkdir -p /opt/st
sudo rm -f /opt/st/*
sudo cp -f "$BIN_DIR/st-0.9."[0-9] /opt/st/
sudo ln -sf /opt/st/st-0.9.* /usr/local/bin/st
sudo -k
echo "Created symlink for st"

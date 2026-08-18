#!/bin/sh

BUILD_DIR=~/repos/dotfiles/install/build/xmonad
BIN_DIR=$BUILD_DIR/bin

sudo mkdir -p /opt/xmonad
for file in /opt/xmonad/*; do
    case "$file" in
        *.bak) ;;
        *) [ -e "$file" ] && sudo mv "$file" "${file}.bak" ;;
    esac
done
sudo cp -f "$BIN_DIR/xmonad-0.18."[0-9] /opt/xmonad/
LATEST_XMONAD=$(ls -v /opt/xmonad/xmonad-0.18.[0-9] | grep -v '\.bak$' | tail -n 1)
if [ -n "$LATEST_XMONAD" ]; then
    sudo ln -sf "$LATEST_XMONAD" /usr/local/bin/xmonad
    echo "Created symlink for xmonad -> $LATEST_XMONAD"
fi
sudo -k

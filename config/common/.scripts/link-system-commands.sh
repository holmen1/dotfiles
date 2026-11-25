#!/bin/sh

set -e

CONFIG_DIR=$HOME/repos/dotfiles/config
sudo ln -sf "$CONFIG_DIR/common/.scripts/dmenu-menu.sh" /usr/local/bin/dmenu-menu
sudo ln -sf "$CONFIG_DIR/xkb/.scripts/xkb-toggle.sh" /usr/local/bin/xkb-toggle

#!/bin/sh

DOTFILES_DIR=$HOME/repos/dotfiles
. "$DOTFILES_DIR/install/hosts/scripts/host-profile.sh"
if ! select_host; then
    echo "Error: Wi-Fi monitoring is not supported on this OS or distribution" >&2
    exit 1
fi

exec "$OS_SCRIPTS_DIR/monitor-wifi.sh" "$@"

#!/bin/sh

set -e

. "$HOME/repos/dotfiles/config/common/.scripts/host-profile.sh"
if ! select_host; then
    echo "Error: service enablement is not supported on this OS or distribution" >&2
    exit 1
fi
load_host_profile

set -f
for service in ${services-}; do
    if [ "$OS" = FreeBSD ]; then
        sudo sysrc "${service}_enable=YES"
    else
        sudo rc-update add "$service" default
    fi
done

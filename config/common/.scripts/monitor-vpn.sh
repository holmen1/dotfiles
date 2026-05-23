#!/bin/sh
# monitor-vpn: A suckless script to monitor and manage Mullvad VPN.

# --- Functions ---
get_location() {
    status=$(mullvad status)
    echo "$status" | grep -q "Connected" && echo "$status" | grep "Relay:" | awk '{print $2}' | cut -d'-' -f1 || echo ""
}

set_location() {
    [ -z "$1" ] && exit 1
    mullvad relay set location "$@"
}

status() {
    mullvad status
}

connect() {
    mullvad connect
}

disconnect() {
    mullvad disconnect
}

help() {
    mullvad help
}

# --- Main ---
case "$1" in
    --get-location)
        get_location
        ;;
    --set-location)
        shift
        set_location "$@"
        ;;
    --status)
        status
        ;;
    --connect)
        connect
        ;;
    --disconnect)
        disconnect
        ;;
    --help)
        help
        ;;
esac

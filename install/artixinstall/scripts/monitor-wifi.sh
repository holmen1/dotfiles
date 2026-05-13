#!/bin/sh
# monitor-wifi: Monitor and manage WiFi via iwd (iwctl).
# TODO: verify on artix — iwd behaviour should be identical to arch
echo "$(date): $0" >> ~/monitor.log

notify() {
    title="$1"; msg="$2"
    #printf '%s' "$msg" | dmenu -p "$title:" >/dev/null 2>&1 &
    xmessage -title "$title" -timeout 5 "$msg"
}

IFACE=$(iwctl device list 2>/dev/null | awk '/station/{print $2}')
INFO=$(iwctl station "$IFACE" show 2>/dev/null)
STATE=$(printf '%s' "$INFO" | awk '/State/{print $2}')
POWERED=$(printf '%s' "$INFO" | awk '/Powered/{print $2}')

if [ -z "$1" ]; then
    if [ -z "$IFACE" ]; then
        notify "Network" "No wireless interface found"
        exit 0
    fi
    if [ "$STATE" != "connected" ]; then
        if [ "$POWERED" = "on" ]; then
            notify "WiFi Help" "WiFi enabled but not connected"
        else
            notify "Network" "WiFi is disabled"
        fi
    fi
    exit 0
fi

get_ssid() {
    if [ -z "$IFACE" ]; then echo ""; exit 1; fi
    if [ "$STATE" != "connected" ]; then echo ""; exit 1; fi
    printf '%s' "$INFO" | awk '/Connected network/{print $3}'
}

scan() {
    if [ -z "$IFACE" ]; then echo "No wireless interface found"; exit 1; fi
    iwctl station "$IFACE" scan >/dev/null 2>&1
    sleep 1
    iwctl station "$IFACE" get-networks 2>/dev/null \
        | sed 's/\x1b\[[0-9;]*m//g' | tail -n +3 | grep -v '^ *-' | grep -v '^$' | awk '{print $1}'
}

connect() {
    SSID="$1"; PASS="$2"
    if [ -z "$IFACE" ] || [ -z "$SSID" ]; then exit 1; fi
    if [ -n "$PASS" ]; then
        iwctl --passphrase "$PASS" station "$IFACE" connect "$SSID" 2>&1
    else
        iwctl station "$IFACE" connect "$SSID" 2>&1
    fi
    sleep 1
    if iwctl station "$IFACE" show 2>/dev/null | awk '/State/{print $2}' | grep -q connected; then
        notify "WiFi" "Connected to $SSID"
    else
        notify "WiFi" "Failed to connect to $SSID"
    fi
}

disconnect() {
    if [ -z "$IFACE" ]; then exit 1; fi
    iwctl station "$IFACE" disconnect
    notify "WiFi" "Disconnected"
}

restart() {
    if [ -z "$IFACE" ]; then exit 1; fi
    sudo rc-service iwd restart
    notify "WiFi" "Restarting"
}

help() {
    printf "iwctl station wlan0 scan\niwctl station wlan0 get-networks\niwctl --passphrase pwd station wlan0 connect network"
}

case "$1" in
    --get-ssid)    get_ssid ;;
    --scan)        scan ;;
    --connect)     connect "$2" "$3" ;;
    --disconnect)  disconnect ;;
    --restart)     restart ;;
    --help)        help ;;
esac

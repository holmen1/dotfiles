#!/bin/sh
# monitor-wifi: Monitor and manage WiFi via NetworkManager (nmcli).

IFACE=$(nmcli -t -f DEVICE,TYPE device | awk -F: '$2=="wifi"{print $1; exit}')
STATE=$(nmcli -t -f DEVICE,STATE device | awk -F: -v iface="$IFACE" '$1==iface{print $2; exit}')

# Monitor WiFi if no argument provided
if [ -z "$1" ]; then
    if [ -z "$IFACE" ]; then
        notify-send -u normal "Network" "No wireless interface found" -i network-wireless-offline
        exit 0
    fi

    if [ "$STATE" != "connected" ]; then
        RADIO=$(nmcli radio wifi)
        if [ "$RADIO" = "enabled" ]; then
            notify-send -u normal -t 150000 "WiFi Help" "WiFi enabled but not connected" -i network-wireless-disconnected
        else
            notify-send -u critical "Network" "WiFi is disabled" -i network-wireless-offline
        fi
    fi
    exit 0
fi

# --- Functions ---
get_ssid() {
    nmcli -t -f ACTIVE,SSID dev wifi | awk -F: '$1=="yes"{print $2; exit}'
}

scan() {
    nmcli -t -f SSID dev wifi list ifname "$IFACE" 2>/dev/null | awk -F: 'NF && $1!=""{print $1}' | sort -u
}

connect() {
    SSID="$1"
    PASS="$2"
    [ -z "$SSID" ] && exit 1
    if [ -n "$PASS" ]; then
        nmcli device wifi connect "$SSID" password "$PASS" ifname "$IFACE" 2>&1
    else
        nmcli device wifi connect "$SSID" ifname "$IFACE" 2>&1
    fi
    NEW_STATE=$(nmcli -t -f DEVICE,STATE device | awk -F: -v iface="$IFACE" '$1==iface{print $2; exit}')
    [ "$NEW_STATE" = "connected" ] && notify-send "WiFi" "Connected to $SSID"
}

disconnect() {
    nmcli device disconnect "$IFACE"
    notify-send "WiFi" "Disconnected"
}

restart() {
    nmcli radio wifi off
    sleep 1
    nmcli radio wifi on
    notify-send "WiFi" "Restarting"
}

help() {
    printf "nmcli device wifi list\nnmcli device wifi connect <SSID> password <pass>\nnmcli device disconnect %s\nnmcli radio wifi on|off\n" "$IFACE"
}

# --- Main ---
case "$1" in
    --get-ssid)    get_ssid ;;
    --scan)        scan ;;
    --connect)     connect "$2" "$3" ;;
    --disconnect)  disconnect ;;
    --restart)     restart ;;
    --help)        help ;;
esac

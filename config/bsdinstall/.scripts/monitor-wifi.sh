#!/bin/sh
# monitor-wifi: A suckless script to monitor and manage WiFi.


# Determine WiFi interface
IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)

# Set WiFi info and state
# FreeBSD)
INFO=$(ifconfig "$IFACE" 2>/dev/null)
STATE=$(printf '%s' "$INFO" | awk '/status:/{print $2}' | sed 's/associated/connected/')
POWERED=$(printf '%s' "$INFO" | grep -q 'flags=.*\bUP\b' && echo "on" || echo "off")

# Monitor WiFi if no argument provided
if [ -z "$1" ]; then
    if [ -z "$IFACE" ]; then
        notify-send -u normal "Network" "No wireless interface found" -i network-wireless-offline
        exit 0
    fi

    if [ "$STATE" != "connected" ]; then
        if [ "$POWERED" = "on" ]; then
            notify-send -u normal -t 150000 "WiFi Help" "WiFi enabled but not connected" -i network-wireless-disconnected
        else
            notify-send -u critical "Network" "WiFi is disabled" -i network-wireless-offline
        fi
    fi
    exit 0
fi

# --- Functions for dmenu ---
get_ssid() {
    if [ -z "$IFACE" ]; then echo ""; exit 1; fi
    if [ "$STATE" != "connected" ]; then echo ""; exit 1; fi
    printf '%s' "$INFO" | awk '/ssid/{print $2}' | tr -d '"'
}

scan() {
    if [ -z "$IFACE" ]; then echo "No wireless interface found"; exit 1; fi
    sudo ifconfig "$IFACE" up >/dev/null 2>&1
    sleep 1
    sudo ifconfig "$IFACE" scan 2>/dev/null | awk 'NR>1 && $1 !~ /^0x/ {print $1}' | sort -u
}

connect() {
    SSID="$1"
    PASS="$2"
    if [ -z "$IFACE" ] || [ -z "$SSID" ]; then exit 1; fi
    NET_ID=$(wpa_cli list_networks 2>/dev/null | awk -F'\t' -v s="$SSID" '$2==s{print $1}')
    if [ -z "$NET_ID" ]; then
        NET_ID=$(wpa_cli add_network | tail -1)
        wpa_cli set_network "$NET_ID" ssid "\"$SSID\"" >/dev/null
        wpa_cli set_network "$NET_ID" psk "\"$PASS\"" >/dev/null
    fi
    wpa_cli select_network "$NET_ID" >/dev/null
    sudo ifconfig "$IFACE" down && sleep 1 && sudo ifconfig "$IFACE" up
    i=0
    while [ $i -lt 15 ]; do
        if wpa_cli status | grep -q 'wpa_state=COMPLETED'; then
            notify-send "WiFi" "Connected to $SSID"
            break
	fi
        sleep 1; i=$((i + 1))
    done
}

disconnect() {
    if [ -z "$IFACE" ]; then exit 1; fi
    wpa_cli disconnect >/dev/null
    notify-send "WiFi" "Disconnected"
}

restart() {
    if [ -z "$IFACE" ]; then exit 1; fi
    sudo service netif restart "$IFACE"
    notify-send "WiFi" "Restarting"
}

help() {
    printf "ifconfig wlan0 up list scan\nwpa_cli add_network\nwpa_cli set_network <id> ssid \"name\"\nwpa_cli set_network <id> psk \"pass\"\nwpa_cli select_network <id>\n\nservice netif restart wlan0"
}

# --- Main ---
case "$1" in
    --get-ssid)
        get_ssid
        ;;
    --scan)
        scan
        ;;
    --connect)
        connect "$2" "$3"
        ;;
    --disconnect)
        disconnect
        ;;
    --restart)
        restart
        ;;
    --help)
        help
        ;;
esac


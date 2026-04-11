#!/bin/sh
# monitor-wifi: A suckless script to monitor and manage WiFi.

OS=$(uname -s)

# Determine WiFi interface
case "$OS" in
    Linux)   IFACE=$(iwctl device list 2>/dev/null | awk '/station/{print $2}') ;;
    FreeBSD) IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1) ;;
esac

# Monitor WiFi if no argument provided
if [ -z "$1" ]; then
    if [ -z "$IFACE" ]; then
        notify-send -u normal "Network" "No wireless interface found" -i network-wireless-offline
        exit 0
    fi

    case "$OS" in
        Linux)
            INFO=$(iwctl station "$IFACE" show 2>/dev/null)
            STATE=$(printf '%s' "$INFO" | awk '/State/{print $2}')
            if [ "$STATE" != "connected" ]; then
                if printf '%s' "$INFO" | awk '/Powered/{print $2}' | grep -q "on"; then
                    notify-send -u normal -t 150000 "WiFi Help" "WiFi enabled but not connected" -i network-wireless-disconnected
                else
                    notify-send -u critical "Network" "WiFi is disabled" -i network-wireless-offline
                fi
            fi
            ;;
        FreeBSD)
            INFO=$(ifconfig "$IFACE" 2>/dev/null)
            STATUS=$(printf '%s' "$INFO" | awk '/status:/{print $2}')
            if [ "$STATUS" != "associated" ]; then
                if printf '%s' "$INFO" | grep -q 'flags=.*\bUP\b'; then
                    notify-send -u normal -t 150000 "WiFi Help" "WiFi enabled but not connected" -i network-wireless-disconnected
                else
                    notify-send -u critical "Network" "WiFi is disabled" -i network-wireless-offline
                fi
            fi
            ;;
    esac
    exit 0
fi

# --- Functions for dmenu ---
get_ssid() {
    if [ -z "$IFACE" ]; then echo ""; exit 1; fi
    case "$OS" in
        Linux)
            iwctl station "$IFACE" show 2>/dev/null | awk '/Connected network/{print $3}'
            ;;
        FreeBSD)
            ifconfig "$IFACE" | awk '/ssid/{print $2}'
            ;;
    esac
}

scan() {
    if [ -z "$IFACE" ]; then echo "No wireless interface found"; exit 1; fi
    case "$OS" in
        Linux)
            iwctl station "$IFACE" scan >/dev/null 2>&1
            sleep 1
            iwctl station "$IFACE" get-networks 2>/dev/null \
                | sed 's/\x1b\[[0-9;]*m//g' | tail -n +3 | grep -v '^ *-' | grep -v '^$' | awk '{print $1}'
            ;;
        FreeBSD)
            sudo ifconfig "$IFACE" up list scan 2>/dev/null | awk 'NR>1 && $1 !~ /^0x/ {print $1}' | sort -u
            ;;
    esac
}

connect() {
    SSID="$1"
    PASS="$2"
    if [ -z "$IFACE" ] || [ -z "$SSID" ]; then exit 1; fi
    case "$OS" in
        Linux)
            if [ -n "$PASS" ]; then
                iwctl --passphrase "$PASS" station "$IFACE" connect "$SSID" 2>&1
            else
                iwctl station "$IFACE" connect "$SSID" 2>&1
            fi
            sleep 1
            iwctl station "$IFACE" show 2>/dev/null | awk '/State/{print $2}' | grep -q connected \
                && notify-send "WiFi" "Connected to $SSID"
            ;;
        FreeBSD)
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
            ;;
    esac
}

disconnect() {
    if [ -z "$IFACE" ]; then exit 1; fi
    case "$OS" in
        Linux)   iwctl station "$IFACE" disconnect ;;
        FreeBSD) wpa_cli disconnect >/dev/null ;;
    esac
    notify-send "WiFi" "Disconnected"
}

help() {
    case "$OS" in
        Linux)
            printf "iwctl station wlan0 scan\niwctl station wlan0 get-networks\niwctl --passphrase pwd station wlan0 connect network"
            ;;
        FreeBSD)
            printf "ifconfig wlan0 up list scan\nwpa_cli add_network\nwpa_cli set_network <id> ssid \"name\"\nwpa_cli set_network <id> psk \"pass\"\nwpa_cli select_network <id>\n\nservice netif restart wlan0"
            ;;
    esac
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
    --help)
        help
        ;;
esac


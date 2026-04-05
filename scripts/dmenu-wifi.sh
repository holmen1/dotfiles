#!/bin/sh
# dmenu-wifi: WiFi manager via dmenu. Run interactively or with --monitor for daemon use (notify on disconnect).

fc-list | grep -qi "JetBrainsMono Nerd Font" \
    && FONT="JetBrainsMono Nerd Font Mono-14" \
    || FONT="monospace-14"

case "$(uname -s)" in
    FreeBSD*) OS="freebsd" ;;
    *)        OS="linux" ;;
esac

menu() { dmenu -p "$1" ${2:+-l "$2"} -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT"; }

scan_networks() {
    case "$OS" in
        linux)
            iwctl station "$IFACE" scan >/dev/null 2>&1
            sleep 1
            iwctl station "$IFACE" get-networks 2>/dev/null \
                | sed 's/\x1b\[[0-9;]*m//g' | tail -n +3 | grep -v '^ *-' | grep -v '^$' | awk '{print $1}'
            ;;
        freebsd) sudo ifconfig "$IFACE" up list scan 2>/dev/null | awk 'NR>1{print $1}' | sort -u ;;
    esac
}

connect_network() {
    case "$OS" in
        linux)
            if [ -n "$2" ]; then
                iwctl --passphrase "$2" station "$IFACE" connect "$1" 2>&1
            else
                iwctl station "$IFACE" connect "$1" 2>&1
            fi
            sleep 1
            iwctl station "$IFACE" show 2>/dev/null | awk '/State/{print $2}' | grep -q connected \
                && notify-send "WiFi" "Connected to $1"
            ;;
        freebsd)
            NET_ID=$(wpa_cli -i "$IFACE" list_networks 2>/dev/null | awk -F'\t' -v s="$1" '$2==s{print $1}')
            if [ -n "$NET_ID" ]; then
                wpa_cli -i "$IFACE" select_network "$NET_ID" >/dev/null
            else
                NET_ID=$(wpa_cli -i "$IFACE" add_network | tail -1)
                wpa_cli -i "$IFACE" set_network "$NET_ID" ssid "\"$1\"" >/dev/null
                wpa_cli -i "$IFACE" set_network "$NET_ID" psk "\"$2\"" >/dev/null
                wpa_cli -i "$IFACE" enable_network "$NET_ID" >/dev/null
                wpa_cli -i "$IFACE" save_config >/dev/null
            fi
            wpa_cli -i "$IFACE" reassociate >/dev/null
            i=0; while [ $i -lt 10 ]; do
                ifconfig "$IFACE" 2>/dev/null | awk '/status:/{print $2}' | grep -q associated && break
                sleep 1; i=$((i+1))
            done
            sudo kill $(cat /var/run/dhclient."$IFACE".pid 2>/dev/null) 2>/dev/null
            sudo dhclient "$IFACE" >/dev/null 2>&1
            ifconfig "$IFACE" 2>/dev/null | awk '/status:/{print $2}' | grep -q associated \
                && notify-send "WiFi" "Connected to $1"
            ;;
    esac
}

disconnect_network() {
    case "$OS" in
        linux)   iwctl station "$IFACE" disconnect ;;
        freebsd) wpa_cli disconnect >/dev/null ;;
    esac
    notify-send "WiFi" "Disconnected"
}

monitor_mode() {
    [ "$status" = "no-interface" ] \
        && notify-send -u normal "Network" "No wireless interface found" -i network-wireless-offline \
        && return
    [ "$status" = "disconnected" ] || return
    case "$OS" in
        linux)
            RADIO=$(iwctl device list 2>/dev/null | grep "$IFACE" | awk '{print $4}')
            if [ "$RADIO" = "on" ]; then
                notify-send -u normal -t 150000 "WiFi Help" "WiFi enabled but not connected" -i network-wireless-disconnected
            else
                notify-send -u critical "Network" "WiFi is disabled" -i network-wireless-offline
            fi
            ;;
        freebsd) ;; # TODO
    esac
}

scan_and_connect() {
    selected=$(scan_networks | menu "Networks:")
    [ -n "$selected" ] && connect_network "$selected" ""
}

# Main
case "$OS" in
    linux)   IFACE=$(iwctl device list 2>/dev/null | awk '/station/{print $2}') ;;
    freebsd) IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1) ;;
esac

if [ -z "$IFACE" ]; then
    status="no-interface"
else
    case "$OS" in
        linux)
            INFO=$(iwctl station "$IFACE" show 2>/dev/null)
            STATE=$(printf '%s' "$INFO" | awk '/State/{print $2}')
            [ "$STATE" = "connected" ] \
                && status="connected:$(printf '%s' "$INFO" | awk '/Connected network/{print $3}')" \
                || status="disconnected"
            ;;
        freebsd)
            INFO=$(ifconfig "$IFACE" 2>/dev/null)
            STATE=$(printf '%s' "$INFO" | awk '/status:/{print $2}')
            SSID=$(printf '%s' "$INFO" | awk '/ssid/{print $2}')
            [ "$STATE" = "associated" ] && [ -n "$SSID" ] && status="connected:$SSID" || status="disconnected"
            ;;
    esac
fi

[ "$1" = "--monitor" ] && { monitor_mode; exit 0; }

case "$status" in
    connected:*)
        SSID=${status#connected:}
        action=$(printf "Disconnect\nScan" | menu "WiFi [$SSID]:")
        case "$action" in
            Disconnect) disconnect_network ;;
            Scan)       scan_and_connect ;;
        esac
        ;;
    *)
        action=$(printf "Scan\nManual\nHelp" | menu "WiFi:")
        case "$action" in
            Scan) scan_and_connect ;;
            Manual)
                ssid=$(echo "" | menu "SSID:")
                if [ -n "$ssid" ]; then
                    passwd=$(menu "Password for $ssid:" </dev/null)
                    connect_network "$ssid" "$passwd"
                fi
                ;;
            Help)
                case "$OS" in
                    linux)
                        printf "iwctl station wlan0 scan\niwctl station wlan0 get-networks\niwctl --passphrase pwd station wlan0 connect network" \
                            | menu "WiFi Help" 3
                        ;;
                    freebsd)
                        printf "Scan:\nsudo ifconfig wlan0 up list scan\n\nConnect:\nNETID=\$(sudo wpa_cli add_network | tail -1)\nsudo wpa_cli set_network \$NETID ssid \"SSID\"\nsudo wpa_cli set_network \$NETID psk \"password\"\nsudo wpa_cli enable_network \$NETID\nsudo wpa_cli save_config\nsudo service netif restart" \
                            | menu "WiFi Help" 10
                        ;;
                esac
                ;;
        esac
        ;;
esac


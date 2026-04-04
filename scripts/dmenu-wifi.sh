#!/bin/sh
# dmenu-wifi: Manage WiFi connections

# Detect OS
case "$(uname -s)" in
    FreeBSD*) OS="freebsd" ;;
    Linux*)   OS="linux" ;;
    *)        OS="unknown" ;;
esac

# Font detection
if fc-list | grep -qi "JetBrainsMono Nerd Font"; then
    FONT="JetBrainsMono Nerd Font Mono-14"
else
    FONT="monospace-14"
fi

# Get WiFi status based on OS
get_status() {
    case "$OS" in
        freebsd)
            IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
            if [ -z "$IFACE" ]; then
                echo "disconnected (no interface)"
                return
            fi
            STATE=$(ifconfig "$IFACE" 2>/dev/null | grep "status:" | awk '{print $2}')
            SSID=$(ifconfig "$IFACE" 2>/dev/null | grep "ssid" | awk -F'"' '{print $2}')
            if [ "$STATE" = "associated" ] && [ -n "$SSID" ]; then
                echo "connected: $SSID"
            else
                echo "disconnected"
            fi
            ;;
        linux)
            IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
            if [ -z "$IFACE" ]; then
                echo "disconnected (no interface)"
                return
            fi
            STATE=$(iwctl station "$IFACE" show 2>/dev/null | grep "State" | awk '{print $2}')
            if [ "$STATE" = "connected" ]; then
                SSID=$(iwctl station "$IFACE" show 2>/dev/null | grep "Connected network" | awk '{print $3}')
                echo "connected: $SSID"
            else
                echo "disconnected"
            fi
            ;;
    esac
}

status=$(get_status)

# Parse connection state
if echo "$status" | grep -q "^connected:"; then
    SSID=$(echo "$status" | cut -d: -f2 | xargs)
    action=$(printf "Disconnect\nSwitch" | dmenu -p "WiFi ($SSID):" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$action" in
        Disconnect)
            case "$OS" in
                linux)
                    IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
                    iwctl station "$IFACE" disconnect
                    notify-send -u normal "WiFi" "Disconnected from $SSID"
                    ;;
                freebsd)
                    IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
                    sudo ifconfig "$IFACE" down
                    notify-send -u normal "WiFi" "Disconnected"
                    ;;
            esac
            ;;
        Switch)
            case "$OS" in
                linux)
                    IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
                    iwctl station "$IFACE" scan >/dev/null 2>&1
                    sleep 1
                    networks=$(iwctl station "$IFACE" get-networks 2>/dev/null | sed 's/\x1b\[[0-9;]*m//g' | tail -n +3 | grep -v '^ *-' | grep -v '^$' | awk '{print $1}')
                    selected=$(echo "$networks" | dmenu -p "Available Networks:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
                    if [ -n "$selected" ]; then
                        passwd=$(echo "" | dmenu -p "Password for $selected:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" -mask '*')
                        if [ -n "$passwd" ]; then
                            iwctl --passphrase "$passwd" station "$IFACE" connect "$selected"
                            sleep 2
                            if iwctl station "$IFACE" show 2>/dev/null | grep -q "State *connected"; then
                                notify-send -u normal "WiFi" "Connected to $selected"
                            else
                                notify-send -u critical "WiFi" "Failed to connect to $selected"
                            fi
                        fi
                    fi
                    ;;
                freebsd)
                    IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
                    sudo ifconfig "$IFACE" up list scan >/dev/null 2>&1
                    networks=$(sudo ifconfig "$IFACE" list scan 2>/dev/null | awk '{print $1}' | grep -v SSID | sort -u)
                    selected=$(echo "$networks" | dmenu -p "Available Networks:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
                    if [ -n "$selected" ]; then
                        passwd=$(echo "" | dmenu -p "Password for $selected:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" -mask '*')
                        if [ -n "$passwd" ]; then
                            NETID=$(sudo wpa_cli add_network 2>/dev/null | tail -1)
                            sudo wpa_cli set_network "$NETID" ssid "\"$selected\"" >/dev/null
                            sudo wpa_cli set_network "$NETID" psk "\"$passwd\"" >/dev/null
                            sudo wpa_cli enable_network "$NETID" >/dev/null
                            sudo wpa_cli save_config >/dev/null
                            notify-send -u normal "WiFi" "Connecting to $selected..."
                        fi
                    fi
                    ;;
            esac
            ;;
    esac
else
    action=$(printf "Scan\nManual" | dmenu -p "WiFi (disconnected):" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
    case "$action" in
        Scan)
            case "$OS" in
                linux)
                    IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
                    iwctl station "$IFACE" scan >/dev/null 2>&1
                    sleep 1
                    networks=$(iwctl station "$IFACE" get-networks 2>/dev/null | sed 's/\x1b\[[0-9;]*m//g' | tail -n +3 | grep -v '^ *-' | grep -v '^$' | awk '{print $1}')
                    selected=$(echo "$networks" | dmenu -p "Available Networks:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
                    if [ -n "$selected" ]; then
                        passwd=$(echo "" | dmenu -p "Password for $selected:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" -mask '*')
                        if [ -n "$passwd" ]; then
                            iwctl --passphrase "$passwd" station "$IFACE" connect "$selected"
                            sleep 2
                            if iwctl station "$IFACE" show 2>/dev/null | grep -q "State *connected"; then
                                notify-send -u normal "WiFi" "Connected to $selected"
                            else
                                notify-send -u critical "WiFi" "Failed to connect to $selected"
                            fi
                        fi
                    fi
                    ;;
                freebsd)
                    IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
                    sudo ifconfig "$IFACE" up list scan >/dev/null 2>&1
                    networks=$(sudo ifconfig "$IFACE" list scan 2>/dev/null | awk '{print $1}' | grep -v SSID | sort -u)
                    selected=$(echo "$networks" | dmenu -p "Available Networks:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
                    if [ -n "$selected" ]; then
                        passwd=$(echo "" | dmenu -p "Password for $selected:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" -mask '*')
                        if [ -n "$passwd" ]; then
                            NETID=$(sudo wpa_cli add_network 2>/dev/null | tail -1)
                            sudo wpa_cli set_network "$NETID" ssid "\"$selected\"" >/dev/null
                            sudo wpa_cli set_network "$NETID" psk "\"$passwd\"" >/dev/null
                            sudo wpa_cli enable_network "$NETID" >/dev/null
                            sudo wpa_cli save_config >/dev/null
                            notify-send -u normal "WiFi" "Connecting to $selected..."
                        fi
                    fi
                    ;;
            esac
            ;;
        Manual)
            ssid=$(echo "" | dmenu -p "SSID:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT")
            if [ -n "$ssid" ]; then
                passwd=$(echo "" | dmenu -p "Password:" -nb "#222222" -nf "#ffffff" -sb "#A300A3" -sf "#ffffff" -fn "$FONT" -mask '*')
                case "$OS" in
                    linux)
                        IFACE=$(iwctl device list 2>/dev/null | grep 'station' | awk '{print $2}')
                        iwctl --passphrase "$passwd" station "$IFACE" connect "$ssid"
                        sleep 2
                        if iwctl station "$IFACE" show 2>/dev/null | grep -q "State *connected"; then
                            notify-send -u normal "WiFi" "Connected to $ssid"
                        else
                            notify-send -u critical "WiFi" "Failed to connect to $ssid"
                        fi
                        ;;
                    freebsd)
                        IFACE=$(ifconfig -l 2>/dev/null | tr ' ' '\n' | grep '^wlan' | head -1)
                        NETID=$(sudo wpa_cli add_network 2>/dev/null | tail -1)
                        sudo wpa_cli set_network "$NETID" ssid "\"$ssid\"" >/dev/null
                        sudo wpa_cli set_network "$NETID" psk "\"$passwd\"" >/dev/null
                        sudo wpa_cli enable_network "$NETID" >/dev/null
                        sudo wpa_cli save_config >/dev/null
                        notify-send -u normal "WiFi" "Connecting to $ssid..."
                        ;;
                esac
            fi
            ;;
    esac
fi

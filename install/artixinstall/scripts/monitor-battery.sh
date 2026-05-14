#!/bin/sh

THRESHOLD=5
WARNING=20

notify() {
    title="$1"; msg="$2"
    xmessage -title "$title" -timeout 5 "$msg"
}

battery_level=$(cat /sys/class/power_supply/BAT*/capacity 2>/dev/null || echo "0")

if [ "$1" = "--get-level" ]; then
    echo $battery_level
    exit 0
fi

if [ "$battery_level" -le $THRESHOLD ]; then
    notify "Battery Critical" "Battery level is ${battery_level}%. Connect charger now!" 
elif [ "$battery_level" -le $WARNING ]; then
    notify "Battery Low" "Battery level is ${battery_level}%"
fi

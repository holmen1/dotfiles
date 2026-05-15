#!/bin/sh

# DEBUG
#echo "$(date): $0" >> ~/monitor.log

THRESHOLD=5
WARNING=20

battery_level=$(cat /sys/class/power_supply/BAT*/capacity 2>/dev/null || echo "0")

if [ "$1" = "--get-level" ]; then
    echo $battery_level
    exit 0
fi

if [ "$battery_level" -le $THRESHOLD ]; then
    notify-send -u critical "Battery Critical" "Battery level is ${battery_level}%. Connect charger now!" -i battery-caution
elif [ "$battery_level" -le $WARNING ]; then
    notify-send -u normal "Battery Low" "Battery level is ${battery_level}%" -i battery-low
fi

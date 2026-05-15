#!/bin/sh
# monitor-loop
# Add to .xinitrc with: ~/monitor-loop.sh &

INTERVAL=60	# 1 minute

while true; do
	$HOME/.scripts/monitor-battery.sh
	$HOME/.scripts/monitor-wifi.sh
	sleep $INTERVAL
done


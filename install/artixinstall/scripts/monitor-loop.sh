#!/bin/sh
# monitor-loop
# Add to .xinitrc with: ~/monitor-loop.sh &

INTERVAL=120	# 2 minutes

while true; do
	$HOME/.scripts/monitor-wifi.sh
	sleep $INTERVAL
done


#!/bin/sh
# Volume mute toggle - works on both Linux and FreeBSD

if [ "$(uname)" = "FreeBSD" ]; then
    # FreeBSD: toggle mute by setting to 0 or restoring (simple version)
    current=$(mixer vol | awk -F: '{print $2}' | awk '{print $1}')
    if [ "$current" = "0" ]; then
        mixer vol 75
    else
        mixer vol 0
    fi
else
    amixer -q set Master toggle
fi

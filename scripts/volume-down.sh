#!/bin/sh
# Volume down - works on both Linux and FreeBSD

if [ "$(uname)" = "FreeBSD" ]; then
    mixer vol -5
else
    amixer -q set Master 5%- unmute
fi

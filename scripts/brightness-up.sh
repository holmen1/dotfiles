#!/bin/sh
# Increase brightness - works on both Linux and FreeBSD

if [ "$(uname)" = "FreeBSD" ]; then
    backlight incr 10
else
    brightnessctl set +10%
fi

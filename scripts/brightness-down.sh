#!/bin/sh
# Decrease brightness - works on both Linux and FreeBSD

if [ "$(uname)" = "FreeBSD" ]; then
    backlight decr 10
else
    brightnessctl set 10%-
fi

#!/bin/sh

# Check for required applications
check_command() {
    command -v "$1" >/dev/null 2>&1 || { echo >&2 "Required program $1 not installed. Aborting."; exit 1; }
}

# Essential applications
check_command xmonad
check_command feh
check_command xcompmgr
check_command brightnessctl
check_command dunst
#check_command st
check_command thunar
check_command iwctl

# [... default stuff that your distro may throw in here ...] #

# Set the default X cursor to the usual pointer
xsetroot -cursor_name left_ptr

# Set a nice background
feh --bg-fill --no-fehbg ~/repos/dotfiles/install/wallpapers/plasma_darkesthour1920x1200.jpg

# Kill any existing notification daemons
killall xfce4-notifyd dunst notify-osd 2>/dev/null || true

# Start dunst notification daemon
dunst &

# Compositor for opacity
xcompmgr &

# Launch key binding daemon if installed (optional)
if command -v xbindkeys >/dev/null 2>&1; then
   xbindkeys &
fi

export TERMINAL=kitty
export BROWSER=firefox

exec xmonad

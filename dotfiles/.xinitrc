#!/bin/sh


# --- Essential applications ---
check_essential() {
    command -v "$1" >/dev/null 2>&1 || { echo >&2 "Required program $1 not installed. Aborting."; exit 1; }
}
check_essential xmonad
check_essential st
check_essential xterm
check_essential iwctl

# --- Optional applications ---
check_optional() {
    command -v "$1" >/dev/null 2>&1 || echo "Warning: Optional program $1 not installed."
}
check_optional xbindkeys
check_optional xsetroot
check_optional feh
check_optional dunst
check_optional xcompmgr
check_optional brightnessctl
check_optional amixer
check_optional thunar
check_optional brave

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

export TERMINAL=st
export BROWSER=brave

exec xmonad

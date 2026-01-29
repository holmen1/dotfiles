#!/bin/sh

# --- Essential applications ---
check_essential() {
    command -v "$1" >/dev/null 2>&1 || { echo >&2 "Required program $1 not installed. Aborting."; exit 1; }
}
check_essential xmonad
check_essential xterm  # Fallback terminal

# --- Optional applications ---
check_optional() {
    command -v "$1" >/dev/null 2>&1 || echo "Warning: Optional program $1 not installed."
}
check_optional st
check_optional xbindkeys
check_optional xsetroot
check_optional feh
check_optional dunst
check_optional xcompmgr

# Set environment variables for applications (must be before any app (lf, xmonad) starts)
export EDITOR=nvim
export TERMINAL=st

# Set browser based on OS
case "$(uname -s)" in
    Linux*)
        export BROWSER=brave
        ;;
    FreeBSD*)
        export BROWSER=librewolf
        ;;
    *)
        export BROWSER=firefox  # Fallback
        ;;
esac

# Set keymap (don't background - fast and should complete before apps start)
setxkbmap se

# Set the default X cursor to the usual pointer
xsetroot -cursor_name left_ptr &

# Set a nice background if feh is available
if command -v feh >/dev/null 2>&1; then
    feh --bg-fill --no-fehbg ~/repos/dotfiles/install/wallpapers/plasma_darkesthour1920x1200.jpg &
fi

# Kill any existing notification daemons and wait briefly
pkill -x dunst 2>/dev/null
pkill -x xfce4-notifyd 2>/dev/null
pkill -x notify-osd 2>/dev/null
sleep 0.1

# Start dunst notification daemon if available
if command -v dunst >/dev/null 2>&1; then
    dunst &
fi

# Start compositor for transparency/shadows
if command -v xcompmgr >/dev/null 2>&1; then
    xcompmgr &
fi

# Launch key binding daemon if available
if command -v xbindkeys >/dev/null 2>&1; then
    xbindkeys &
fi

# Let background processes initialize
sleep 0.2

# Start xmonad (xmonad should handle starting terminals via its config)
exec xmonad

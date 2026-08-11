#!/bin/sh

# --- Essential applications ---
check_essential() {
    command -v "$1" >/dev/null 2>&1 || { echo >&2 "Required program $1 not installed. Aborting."; exit 1; }
}
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

# Set keymap (don't background - fast and should complete before apps start)
setxkbmap se
echo "se" > "$HOME/.cache/xkb-layout" # Cache current layout for dmenu status script

xrandr --auto

# Set a nice background using xwallpaper if available
if command -v xwallpaper >/dev/null 2>&1; then
    xwallpaper --zoom "$HOME/repos/dotfiles/config/common/.scripts/plasma_darkesthour1920x1200.jpg" &
else
    echo "Warning: xwallpaper not installed, wallpaper not set by .xinitrc"
fi

# Start compositor for transparency/shadows 
if command -v xcompmgr >/dev/null 2>&1; then
    xcompmgr &
fi

# Launch key binding daemon if available
if command -v xbindkeys >/dev/null 2>&1; then
    xbindkeys &
fi

exec dwm


#!/bin/sh

. "$HOME/repos/dotfiles/config/common/.scripts/host-profile.sh"
if ! select_host; then
    echo "Error: X session is not supported on this OS or distribution" >&2
    exit 1
fi
load_host_profile || exit 1
if [ -z "${window_manager-}" ] || [ -z "${terminal-}" ] || [ -z "${xkb_layout-}" ]; then
    echo "Error: missing X session configuration in '$HOST_CONFIG'" >&2
    exit 1
fi

# --- Essential applications ---
check_essential() {
    command -v "$1" >/dev/null 2>&1 || echo >&2 "Required program $1 not installed."
}
{
    check_essential "$window_manager"
    check_essential xterm
} 2>&1 | tee ~/.xinitrc-errors.log

# --- Optional applications ---
check_optional() {
    command -v "$1" >/dev/null 2>&1 || echo "Warning: Optional program $1 not installed."
}
check_optional "$terminal"
check_optional xbindkeys
check_optional xsetroot
check_optional feh
check_optional dunst
check_optional xcompmgr

# Set environment variables for applications (must be before any app (lf, xmonad) starts)
export EDITOR=nvim
export TERMINAL="$terminal"

# Set browser: use first installed from preference list
for _b in brave chromium librewolf firefox chrome; do
    if command -v "$_b" >/dev/null 2>&1; then
        export BROWSER="$_b"
        break
    fi
done
[ -z "$BROWSER" ] && echo "Warning: No browser found (brave, chromium, librewolf, firefox)"
unset _b

# Set keymap (don't background - fast and should complete before apps start)
if ! setxkbmap "$xkb_layout"; then
    echo >&2 "Failed to set XKB layout: $xkb_layout"
    exit 1
fi
mkdir -p "$HOME/.cache"
echo "$xkb_layout" > "$HOME/.cache/xkb-layout"

# Set the default X cursor to the usual pointer
xsetroot -cursor_name left_ptr &

xrandr --auto

# Set a nice background using xwallpaper if available
if command -v xwallpaper >/dev/null 2>&1; then
    xwallpaper --zoom ~/.config/wallpapers/plasma_darkesthour1920x1200.jpg &
else
    echo "Warning: xwallpaper not installed, wallpaper not set by .xinitrc"
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

# Start battery/wifi monitors in background
[ -x "$HOME/.scripts/monitor-loop.sh" ] && $HOME/.scripts/monitor-loop.sh &

# Let background processes initialize
sleep 0.2

exec "$window_manager"

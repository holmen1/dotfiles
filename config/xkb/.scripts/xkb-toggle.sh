#!/bin/sh
# Minimal XKB toggle script

KEYMAP="$HOME/.cache/custom-keymap.xkb"
STATE="$HOME/.cache/xkb-layout"

[ -f "$STATE" ] || { echo "Warning: $STATE not found; start X before toggling layouts." >&2; exit 1; }

if [ "$(cat "$STATE")" = "custom" ]; then
    . "$HOME/repos/dotfiles/install/hosts/scripts/host-profile.sh"
    if ! select_host; then
        echo "Error: XKB toggle is not supported on this OS or distribution" >&2
        exit 1
    fi
    load_host_profile || exit 1
    [ -n "${xkb_layout-}" ] || { echo "Error: XKB layout missing from '$HOST_CONFIG'" >&2; exit 1; }
    pkill -x xcape 2>/dev/null || true
    setxkbmap "$xkb_layout" || { echo >&2 "Failed to restore XKB layout: $xkb_layout"; exit 1; }
    echo "$xkb_layout" > "$STATE"
else
    if ! xkbcomp -w0 "$KEYMAP" "$DISPLAY"; then
        echo >&2 "Failed to load custom XKB map: $KEYMAP"
        exit 1
    fi
    pkill -x xcape 2>/dev/null || true
    xcape -e 'Control_L=Escape' &
    echo custom > "$STATE"
fi

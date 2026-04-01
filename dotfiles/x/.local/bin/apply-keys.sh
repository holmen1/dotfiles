#!/bin/sh
# apply-keys.sh — portable X11 keyboard customisation (XKB + xcape)
#
# Key behaviour
#   CapsLock : hold = Control,   tap = Escape
#   Space    : hold = num-layer, tap = Space
#
# Num-layer (Space held)
#   u i o  →  7 8 9
#   j k l  →  4 5 6
#   m , .  →  1 2 3
#     n    →  0
#
# Requires: setxkbmap, xkbcomp, xcape
# Portable: Arch Linux and FreeBSD with Xlibre/Xorg

# ── 1. Merge layout ────────────────────────────────────────────────────────
# setxkbmap -print emits an XKB keymap description without applying it.
# sed appends +local(numspace) to the symbols include line.
# xkbcomp compiles the merged keymap and pushes it to the X server.
# -I points xkbcomp at the user XKB directory (~/.config/xkb/).
if ! setxkbmap se -option ctrl:nocaps -print \
    | sed '/xkb_symbols/s|include "\(.*\)"|include "\1+local(numspace)"|' \
    | xkbcomp -w0 -I"$HOME/.config/xkb" - "$DISPLAY"; then
    echo "apply-keys.sh: xkb merge failed — applying base layout only" >&2
    setxkbmap se -option ctrl:nocaps
fi

# ── 2. xcape — synthesise key event when modifier is tapped alone ──────────
#   Control_L        (CapsLock remapped) → Escape
#   ISO_Level3_Shift (Space remapped)    → space
if command -v xcape >/dev/null 2>&1; then
    pkill -x xcape 2>/dev/null || true
    sleep 0.1
    xcape -e 'Control_L=Escape;ISO_Level3_Shift=space' &
else
    echo "apply-keys.sh: xcape not found — tap behaviour unavailable" >&2
fi

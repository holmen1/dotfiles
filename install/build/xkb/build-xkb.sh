#!/bin/sh
# build-xkb.sh — compile XKB keymap for X11 keyboard customisation
#
# Produces: dotfiles/xkb/.config/xkb/keymap-linux.xkb or keymap-bsd.xkb
#
# Re-run this whenever dotfiles/xkb/.config/xkb/symbols/local changes.
#
# Deps: setxkbmap, xkbcomp

set -e

DOTFILES="${HOME}/repos/dotfiles"
SYMBOLS_DIR="${DOTFILES}/install/build/xkb"

case "$(uname -s)" in
    FreeBSD*) KEYMAP="${DOTFILES}/dotfiles/xkb/.config/xkb/keymap-bsd.xkb" ;;
    *)        KEYMAP="${DOTFILES}/dotfiles/xkb/.config/xkb/keymap-linux.xkb" ;;
esac

mkdir -p "$(dirname "$KEYMAP")"

setxkbmap se -option ctrl:nocaps,lv3:lwin_switch -print \
    | sed '/xkb_symbols/s|include "\(.*\)"|include "\1+local"|' \
    | xkbcomp -w0 -I"${SYMBOLS_DIR}" - -o "${KEYMAP}"

echo "Keymap written to ${KEYMAP}"

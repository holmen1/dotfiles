#!/bin/sh

# [... default stuff that your distro may throw in here ...] #

# Set the default X cursor to the usual pointer
xsetroot -cursor_name left_ptr

# Set a nice background
feh --bg-fill --no-fehbg ~/repos/dotfiles/wallpapers/haskell.png



# Compositor for opacity
xcompmgr &

# Needed for brightness keybinding
export HOSTNAME="x"

exec xmonad

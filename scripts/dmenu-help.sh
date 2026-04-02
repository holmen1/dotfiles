#!/bin/sh
# Show xmonad keybindings in dmenu
cat <<EOF | dmenu -l 28 -i -p "XMonad Help" \
-nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" \
-fn "JetBrainsMono Nerd Font Mono-16"
Mod+Enter        Terminal
Mod+Shift+Enter  Swap master window
Mod+q            Close window
Mod+Shift+q      Quit xmonad
Mod+[1..4]       Switch to workspace N
Mod+Shift+[1..4] Move window to workspace N and follow
Mod+Tab          Next workspace
Mod+Shift+Tab    Previous workspace
Mod+j            Focus next window
Mod+k            Focus previous window
Mod+h            Shrink master area
Mod+l            Expand master area
Mod+,            Increment master windows
Mod+.            Decrement master windows
Mod+Space        Rotate through available layouts
- Scratchpads
Mod+w            Browser toggle
Mod+p            htop toggle
- Applications
Mod+b            Browser
Mod+e            File manager
Mod+s            Screenshot
Mod+Shift+s      Area screenshot
Mod+a            App launcher (dmenu_run)
- dmenus
Mod+v            Mullvad VPN menu
Mod+x            Logout menu
Mod+z            XKB layout menu
EOF

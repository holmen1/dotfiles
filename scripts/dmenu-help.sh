#!/bin/bash
# Show xmonad keybindings in dmenu
cat <<EOF | dmenu -l 19 -i -p "XMonad Help" \
-nb "#222222" -nf "#ffffff" -sb "#222222" -sf "#ffffff" \
-fn "JetBrainsMono Nerd Font Mono-16"
Mod+Enter        Terminal
Mod+q            Close window
Mod+[1..4]       Switch to workspace N
Mod+Tab          Next workspace
Mod+Shift+[1..4] Move window to workspace N and follow
Mod+Shift+Enter  Swap master window
Mod+Shift+h      Shrink master area
Mod+Shift+l      Expand master area
- Scratchpads
Mod+w            Browser toggle
Mod+z            htop toggle
- Applications
Mod+e            File manager
Mod+s            Screenshot
Mod+Shift+s      Area screenshot
Mod+a            App launcher (dmenu_run)
- dmenus
Mod+v            Mullvad VPN menu
Mod+x            Logout menu
EOF

#!/bin/bash
# Show xmonad keybindings in dmenu
cat <<EOF | dmenu -l 10 -i -p "XMonad Help" -nb "#222222" -nf "#ffffff" -sb "#ff005f" -sf "#ffffff"
Mod+a           App launcher (dmenu_run)
Mod+w           Browser
Mod+e           File manager
Mod+Enter       Terminal
Mod+Shift+Enter Swap master window
Mod+c           Close window
Mod+q           Logout menu
Mod+m           Mullvad VPN menu
Mod+s           Screenshot
Mod+Shift+s     Area screenshot
Mod+Tab         Next workspace
Mod+Shift+h     Shrink master area
Mod+Shift+l     Expand master area
Mod+[1..6]      Switch to workspace N
Mod+Shift+[1..6] Move window to workspace N and follow
EOF

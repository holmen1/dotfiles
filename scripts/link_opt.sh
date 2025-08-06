#!/bin/bash
# filepath: /home/holmen1/repos/dotfiles/scripts/link_opt.sh

echo "Linking binaries from /opt to /usr/local/bin..."

# Define binaries to link: "directory:binary_name:link_name"
binaries=(
    "xmonad:xmonad-v0.18.0:xmonad"
    "st:st-0.9.2:st"
    "VSCode-linux-x64:bin/code:code"
)

for binary in "${binaries[@]}"; do
    IFS=':' read -r dir bin_name link_name <<< "$binary"
    
    if [ -f "/opt/$dir/$bin_name" ]; then
        sudo ln -sf "/opt/$dir/$bin_name" "/usr/local/bin/$link_name"
        echo "✓ Linked $link_name: /opt/$dir/$bin_name -> /usr/local/bin/$link_name"
    fi
done

echo "Done linking /opt binaries to /usr/local/bin"
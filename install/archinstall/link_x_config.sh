#!/bin/bash

BASE_DIR="~/repos/dotfiles"

# Define an array of source and target directories
links=(
    "$BASE_DIR/dotfiles/nvim:~/.config/nvim"
    "$BASE_DIR/dotfiles/.bashrc:~/.bashrc"
    "$BASE_DIR/dotfiles/.bash_profile:~/.bash_profile"
    "$BASE_DIR/dotfiles/.vimrc:~/.vimrc"
    "$BASE_DIR/dotfiles/.xbindkeysrc:~/.xbindkeysrc"
    "$BASE_DIR/dotfiles/.xinitrc:~/.xinitrc"
    "$BASE_DIR/dotfiles/lf:~/.config/lf"
    "$BASE_DIR/scripts/system-monitor.service:~/.config/systemd/user/system-monitor.service"
    "$BASE_DIR/scripts/system-monitor.timer:~/.config/systemd/user/system-monitor.timer"
)

# Create the symbolic links
for link in "${links[@]}"; do
    source="${link%%:*}"
    target="${link##*:}"
    
    # Expand ~ to the home directory
    source_expanded=$(eval echo $source)
    target_expanded=$(eval echo $target)
   
    # Remove the target if it is an existing directory or symlink
    if [ -L "$target_expanded" ]; then
        # It's a symlink, remove it
        rm "$target_expanded"
        echo "Removed existing symlink: $target_expanded"
    elif [ -e "$target_expanded" ]; then
        # It exists (file or directory), back it up first
        backup="${target_expanded}.backup.$(date +%Y%m%d_%H%M%S)"
        mv "$target_expanded" "$backup"
        echo "Backed up existing file/directory: $target_expanded -> $backup"
    fi

    # Create the symbolic link
    mkdir -p "$(dirname "$target_expanded")"
    ln -s "$source_expanded" "$target_expanded"
    if [ $? -eq 0 ]; then
        echo "Created symbolic link: $target_expanded -> $source_expanded"
    else
        echo "Failed to create symbolic link: $target_expanded -> $source_expanded"
    fi
done

sudo ln -sf /opt/st/st-0.9.2 /usr/local/bin/st
sudo ln -sf /opt/xmonad/xmonad-v0.18.0 /usr/local/bin/xmonad
echo "Created symlinks for st and xmonad"

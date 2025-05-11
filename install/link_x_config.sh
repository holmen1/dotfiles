#!/bin/bash

BASE_DIR="~/repos/dotfiles"

# Define an array of source and target directories
links=(
    "$BASE_DIR/dotfiles/xmonad:~/.config/xmonad"
    #"$BASE_DIR/dotfiles/nvim:~/.config/nvim"
    "$BASE_DIR/dotfiles/.bashrc:~/.bashrc"
    "$BASE_DIR/dotfiles/.bash_profile:~/.bash_profile"
    "$BASE_DIR/dotfiles/.xinitrc:~/.xinitrc"
)

# Create the symbolic links
for link in "${links[@]}"; do
    source="${link%%:*}"
    target="${link##*:}"
    
    # Expand ~ to the home directory
    source_expanded=$(eval echo $source)
    target_expanded=$(eval echo $target)
   
    # Remove the target if it is an existing directory or symlink
    if [ -d "$target_expanded" ] || [ -L "$target_expanded" ]; then
        rm -rf "$target_expanded"
    fi

    # Create the symbolic link
    ln -s "$source_expanded" "$target_expanded"
    if [ $? -eq 0 ]; then
        echo "Created symbolic link: $target_expanded -> $source_expanded"
    else
        echo "Failed to create symbolic link: $target_expanded -> $source_expanded"
    fi
done

#!/bin/sh

# Check if a config file is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <config-file>"
  echo "Example: $0 linux_links.conf"
  exit 1
fi

CONFIG_FILE="$1"
BASE_DIR="~/repos/dotfiles"

# Source the configuration file if it exists
if [ -f "$CONFIG_FILE" ]; then
  . "$CONFIG_FILE"
else
  echo "Error: Config file '$CONFIG_FILE' not found!"
  exit 1
fi

# Check if links variable is defined in the config file
if [ -z "$links" ]; then
  echo "Error: No links defined in config file!"
  exit 1
fi

# Create the symbolic links
echo "$links" | while IFS= read -r link; do
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


#!/bin/bash

# Check if the tar file is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <tar-file>"
  exit 1
fi

# Define variables
TAR_FILE="$1"
DEST_DIR="/opt/VSCode-linux-x64"
LINK_PATH="/usr/local/bin/code"

mkdir -p $DEST_DIR
# Extract the tar.gz file
tar -xvzf $TAR_FILE

# Move the extracted directory to /opt
mv VSCode-linux-x64/ $DEST_DIR

# Create a symbolic link
ln -s $DEST_DIR/bin/code $LINK_PATH

echo "Visual Studio Code has been installed and linked successfully."

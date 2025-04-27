#!/bin/bash

# Check if Git email is configured
git_email=$(git config --global user.email)
if [[ -z "$git_email" ]]; then
    echo "Git email is not configured. Please set it using 'git config --global user.email'."
    exit 1
fi

# Ensure the .ssh directory exists
if [[ ! -d ~/.ssh ]]; then
    mkdir -p ~/.ssh
    echo "Created ~/.ssh directory."
fi

# Check if the SSH key already exists
if [[ -f ~/.ssh/id_ed25519 ]]; then
    echo "SSH key already exists at ~/.ssh/id_ed25519. No new key generated."
    exit 0
fi

# Generate SSH key
ssh-keygen -t ed25519 -C "$git_email" -f ~/.ssh/id_ed25519 -N ""

# Start the ssh-agent in the background
eval "$(ssh-agent -s)"

# Add the SSH key to the ssh-agent
ssh-add ~/.ssh/id_ed25519

echo "SSH key generated and added to ssh-agent"

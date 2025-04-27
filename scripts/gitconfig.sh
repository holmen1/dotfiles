#!/bin/bash

# Check if Git already has a global username and email configured
existing_username=$(git config --global user.name)
existing_email=$(git config --global user.email)

if [[ -n "$existing_username" && -n "$existing_email" ]]; then
    echo "Git is already configured with the following global settings:"
    echo "Username: $existing_username"
    echo "Email: $existing_email"
    exit 0
fi

# Prompt for username and email
read -p "Enter your Git username: " git_username
read -p "Enter your Git email: " git_email

# Configure Git
git config --global user.name "$git_username"
git config --global user.email "$git_email"

username=$(git config --global user.name)
email=$(git config --global user.email)

echo "Git is configured with the following global settings:"
echo "Username: $username"
echo "Email: $email"

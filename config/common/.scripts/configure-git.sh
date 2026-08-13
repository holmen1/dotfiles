#!/bin/sh

USER=$(whoami)
EMAIL=$USER@gmail.com

# Configure git
#git config --global user.name "$USER"
#git config --global user.email "$EMAIL"

echo "git global config:"
echo ""
git config list --global


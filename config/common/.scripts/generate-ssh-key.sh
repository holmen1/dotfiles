#!/bin/sh

USER=$(whoami)
EMAIL=$USER@gmail.com

cd $HOME
mkdir -p .ssh
chmod 700 .ssh
ssh-keygen -t ed25519 -C "$EMAIL" -f $HOME/.ssh/id_ed25519 -N ""
eval "$(ssh-agent -s)"
ssh-add $HOME/.ssh/id_ed25519


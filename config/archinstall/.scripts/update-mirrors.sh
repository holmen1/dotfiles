#!/bin/bash
# Save and backup Arch mirrorlist

MIRRORLIST="/etc/pacman.d/mirrorlist"
BACKUP="/etc/pacman.d/mirrorlist.$(date +%Y%m%d-%H%M%S).bak"

# Backup current mirrorlist
sudo cp "$MIRRORLIST" "$BACKUP"

# Update with 10 fastest global HTTPS mirrors
sudo reflector --protocol https --sort rate --latest 10 --save "$MIRRORLIST"

echo "Mirrorlist updated. Backup saved as $BACKUP"
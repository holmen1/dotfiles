# LESSONS LEARNED *(Arch Linux)*

## ⚠️ NEVER Edit /etc/sudoers with vim (or any editor that isn't visudo)

## NetworkManager

For reliable network management, especially with GUI applications and system tray integration:

### Essential Packages
```bash
sudo pacman -S networkmanager network-manager-applet
```

### Service Configuration
Always disable conflicting network services and enable NetworkManager:
```bash
# Disable conflicting services
sudo systemctl disable iwd dhcpcd systemd-networkd
sudo systemctl stop iwd dhcpcd systemd-networkd

# Enable NetworkManager
sudo systemctl enable NetworkManager
sudo systemctl start NetworkManager
```

### Key Benefits
- **GUI Integration**: `nm-applet` provides system tray network management
- **Clean Service State**: No conflicting D-Bus services
- **Waybar Integration**: Network module works reliably without manual interface specification

### ⚠️ Critical Rules
1. **Only run ONE network manager** (NetworkManager OR iwd OR systemd-networkd)
2. **Install the applet** (`network-manager-applet`) for tray functionality
3. **Check service conflicts** with `systemctl --failed` and `systemctl list-units --state=active | grep network`
# keyd Setup and Usage Guide

> **Note:** keyd is Linux-only and does not work on FreeBSD.  
> This repo now uses **XKB + xcape** for portable, daemon-free key remapping.  
> See [`dotfiles/x/README.md`](../x/README.md) for the current setup.

The configuration in `default.conf` is kept for reference (Arch Linux only).

keyd is a key remapping daemon for Linux, allowing you to configure custom keyboard layouts, remap keys, and create advanced input workflows.

## Installation

### Arch Linux (AUR)
```sh
yay -S keyd
```

For other distributions, see the [official keyd repository](https://github.com/rvaiya/keyd).

## Configuration

1. **Edit the configuration file:**
   - The main config is typically located at `/etc/keyd/default.conf`.
   - In this repo, you can find a sample config at `dotfiles/keyd/default.conf`.
   - To use this config, link it to `/etc/keyd/default.conf`:
     ```sh
     sudo ln -s /path/to/dotfiles/keyd/default.conf /etc/keyd/default.conf
     ```

2. **Reload keyd:**
   ```sh
   sudo systemctl restart keyd
   ```

3. **Enable keyd to start on boot:**
   ```sh
   sudo systemctl enable keyd
   ```

## Troubleshooting
- Check the status of keyd:
  ```sh
  keyd check
  ```


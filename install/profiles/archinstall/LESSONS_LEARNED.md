# LESSONS LEARNED

## Update linux-firmware

### Manual Intervention Required (June 2025)

**Important:**
Starting with `linux-firmware >= 20250613.12fe085f-5`, the package was split and the NVIDIA firmware layout changed. Upgrading from an earlier version may cause errors like:

```
linux-firmware-nvidia: /usr/lib/firmware/nvidia/ad103 exists in filesystem
...
```

#### Solution

1. **Download new firmware packages before removing the old one (to avoid losing WiFi):**
    ```
    sudo pacman -Sw linux-firmware
    ```

2. **Remove the old package (this does NOT delete firmware files from disk):**
    ```
    sudo pacman -Rdd linux-firmware
    ```

3. **Upgrade and reinstall:**
    ```
    sudo pacman -Syu linux-firmware
    ```

> **Note:** Removing `linux-firmware` with `-Rdd` only removes the package from the database. The actual firmware files remain on disk, so your WiFi and other hardware should keep working during the upgrade.

**References:**
- [Arch Linux News: linux-firmware upgrade requires manual intervention](https://archlinux.org/news/linux-firmware-20250613-12fe085f-5-upgrade-requires-manual-intervention/)

### If You Lose WiFi: Update with Archinstall ISO

If you lose WiFi connectivity during a `linux-firmware` upgrade and cannot restore it from your installed system, you can use the Arch Linux ISO (live environment) to repair your system:

1. **Boot from the latest Arch Linux ISO (USB/DVD).**

2. **Connect to the internet using a wired (Ethernet) connection if possible.**
   If WiFi is unavailable, Ethernet is the most reliable option.

3. **Mount your root partition and (if needed) other subvolumes:**
    ```
    # mount -o subvol=@ /dev/<root-partition> /mnt
    # mount -o subvol=@home /dev/<root-partition> /mnt/home
    ```
    *(Replace `<root-partition>` with your actual device, e.g., `nvme0n1p2`.)*

4. **Mount other necessary filesystems:**
    ```
    # mount /dev/<efi-partition> /mnt/boot
    # mount --bind /dev /mnt/dev
    # mount --bind /proc /mnt/proc
    # mount --bind /sys /mnt/sys
    ```

5. **Chroot into your installed system:**
    ```
    # arch-chroot /mnt
    ```

6. **Update and reinstall firmware packages:**
    ```
    # pacman -Syu linux-firmware
    ```
    *(Add any other firmware packages you need, e.g., `linux-firmware-nvidia-gsp`.)*

7. **Exit chroot and reboot:**
    ```
    # exit
    # reboot
    ```

Your system should now have the latest firmware and, if supported, WiFi should work again.

---

**Tip:**
If you need to download packages on another machine, you can copy them to `/mnt/var/cache/pacman/pkg/` before installing with `pacman -U`.

---

**References:**
- [Arch Wiki: Chroot](https://wiki.archlinux.org/title/Chroot)
- [Arch Wiki: General Troubleshooting](https://wiki.archlinux.org/title/General_troubleshooting)

### WiFi not found

Brave and VS Code are leaking file descriptors, causing system-wide issues

```bash
iwctl --passphrase xxx station wlan0 connect XX
Device wlan0 not found
```

Checked status
```bash
$ systemctl status iwd
● iwd.service - Wireless service
     Loaded: loaded (/usr/lib/systemd/system/iwd.service; enabled; preset: disabled)
     Active: active (running) since Wed 2025-08-20 14:57:42 CEST; 21min ago
 Invocation: 92b30d00addf4705b0ef05fb3020502a
       Docs: man:iwd(8)
             man:iwd.config(5)
             man:iwd.network(5)
             man:iwd.ap(5)
   Main PID: 1482 (iwd)
      Tasks: 1 (limit: 33075)
     Memory: 2M (peak: 3.2M)
        CPU: 22ms
     CGroup: /system.slice/iwd.service
             └─1482 /usr/lib/iwd/iwd

Aug 20 14:57:43 p16 iwd[1482]:                         Max HE RX <= 80MHz MCS: 0-11 for NSS: 2
Aug 20 14:57:43 p16 iwd[1482]:                         Max HE TX <= 80MHz MCS: 0-11 for NSS: 2
Aug 20 14:57:43 p16 iwd[1482]:                         Max HE RX <= 160MHz MCS: 0-11 for NSS: 2
Aug 20 14:57:43 p16 iwd[1482]:                         Max HE TX <= 160MHz MCS: 0-11 for NSS: 2
Aug 20 14:57:43 p16 iwd[1482]:         Ciphers: BIP-CMAC-256 BIP-GMAC-256 BIP-GMAC-128 CCMP-256
Aug 20 14:57:43 p16 iwd[1482]:                  GCMP-256 GCMP-128 BIP-CMAC-128 CCMP-128
Aug 20 14:57:43 p16 iwd[1482]:                  TKIP
Aug 20 14:57:43 p16 iwd[1482]:         Supported iftypes: station ap p2p-client p2p-go p2p-device
Aug 20 14:57:43 p16 iwd[1482]: NEW_INTERFACE failed: Too many open files in system
Aug 20 14:57:43 p16 iwd[1482]: udev interface=wlan0 ifindex=3
```
**❌ ERROR: Too many open files in system**

#### Monitoring and Recovery

1. Monitor open files regularly:
   ```bash
   lsof | wc -l
   sudo lsof | awk '{print $2}' | sort | uniq -c | sort -nr | head
   ```

2. Identify the leaking process:
   ```bash
   ps -p <pid1>,<pid2>,... -o pid,comm,cmd
   ```
   - Replace `<pid1>,<pid2>,...` with the PIDs from the previous command
   - If you see a process (e.g., `code`, `brave`) with tens of thousands of open files, it may be leaking file descriptors, ie
   ```
   PID COMMAND         CMD
   1868 brave           /opt/brave-bin/brave --password-store=basic
   2939 code            /opt/visual-studio-code/code .
   1642 Hyprland        Hyprland
   ```

3. Recover:
   Save your work, then kill or restart the offending process(es):
     ```bash
     kill <pid>
     ```
   Restart iwd to restore WiFi:
     ```bash
     sudo systemctl restart iwd
     ```
     ⚠️ Only use one network manager at a time (iwd, NetworkManager, or systemd-networkd).
Running multiple network managers can cause conflicts, unpredictable behavior, or loss of connectivity

4. Reconnect to WiFi if needed:
   ```bash
   iwctl station wlan0 connect <NetworkName>
   ```

5. or switch to NetworkManager...



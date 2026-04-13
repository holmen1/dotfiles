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
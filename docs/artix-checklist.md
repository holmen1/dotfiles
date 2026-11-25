# Artix installation checklist

Printable checklist for tracking a manual install. See the [base install guide](artix-install.md)
and [post-install guide](artix-post-install.md). Mark items with
`[x]` on paper or in a local copy — don't commit checked-off state.

## Base installation

- [ ] Boot the OpenRC ISO and select keyboard layout
- [ ] Identify and confirm the target disk
- [ ] Erase disk
- [ ] Partition
- [ ] Create LUKS
- [ ] Create LVM
- [ ] Format
- [ ] Mount
- [ ] Verify network connectivity
- [ ] Update the system clock
- [ ] Install base system
- [ ] Install a kernel
- [ ] Generate fstab
- [ ] chroot
- [ ] Set root password
- [ ] Initialize the pacman keyring
- [ ] Configure system clock
- [ ] Locale
- [ ] Console keymap
- [ ] mkinitcpio.conf
- [ ] Bootloader
- [ ] Add user
- [ ] Network configuration
- [ ] Exit chroot and reboot

## Post-installation

- [ ] Connect to WiFi
- [ ] Configure Git and SSH key, or prepare an existing key
- [ ] Clone dotfiles
- [ ] Update system packages
- [ ] Install Xorg and X utilities; verify `startx`
- [ ] Run general setup and link dotfiles before building XMonad
- [ ] Install GHC, build XMonad libraries and binary, and install XMonad as needed
- [ ] Enable services and run the sanity check

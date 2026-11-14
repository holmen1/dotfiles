# packages

## Remove

To remove a package and its dependencies which are not required by any other installed package:
```bash
pacman -R[n]s package_name
```
`-n` prevent creation of backup files

***LESSONS LEARNED***
Remove dash cause unable to reboot

openrc unable to exec

because sh -> dash

solution: iso boot, relink sh to bash

discovered could not mount cryptroot:unknown filesystem type LVM2_MEMBER
solution:
vgscan found vg0 type lvm2
vgchange -ay

mount /dev/mapper/vg0-root /mnt

## Queries

Query the local package database with the -Q flag, the sync database with the -S
```bash
pacman -Ss string1 string2 ...
```

To list all packages explicitly installed and not required as dependencies:
```bash
pacman -Qet
```

To view the dependency tree of a package:
```bash
pactree package_name
```

## Downgrade from the local pacman cache

Artix uses `pacman`, so downgrading a package is usually done with a cached package file or a manually downloaded older package.

1. Find the package version you want:
   ```bash
   ls /var/cache/pacman/pkg | grep '^package-name-'
   ```

2. Install that exact file:
   ```bash
   sudo pacman -U /var/cache/pacman/pkg/package-name-<version>-<arch>.pkg.tar.zst
   ```

3. If the current version is still installed, pacman will replace it with the selected older package.

## Downgrade from a downloaded package file

If the version is no longer in the cache, download the older package first, then install it with:

```bash
sudo pacman -U ./package-name-<version>-<arch>.pkg.tar.zst
```

## Check the installed version

```bash
pacman -Q package-name
```

## List all packages installed from a given repository.

Usage: paclist <repository ...>
```bash
# Arch repo
paclist extra
```

## Notes

- Keep the old package file if you may need to reinstall it later.
- If a package was built locally or from AUR, downgrade it the same way: use the exact `.pkg.tar.zst` file and `pacman -U`.

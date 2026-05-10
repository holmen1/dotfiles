# packages

Artix uses `pacman`, so downgrading a package is usually done with a cached package file or a manually downloaded older package.

## Downgrade from the local pacman cache

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

## Notes

- Keep the old package file if you may need to reinstall it later.
- If a package was built locally or from AUR, downgrade it the same way: use the exact `.pkg.tar.zst` file and `pacman -U`.

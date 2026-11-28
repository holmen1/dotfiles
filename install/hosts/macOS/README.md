# macOS setup

The `mac02.conf` file selects this host's Stow packages. macOS remains a
manual setup: pass the profile to the shared linker and use the Homebrew
helpers below as needed.

## Basic Homebrew Commands

- **Install a package:**
  ```bash
  brew install <package-name>
  ```
- **Remove a package:**
  ```bash
  brew uninstall <package-name>
  ```

- **Update Homebrew itself:**
  ```bash
  brew update
  ```

- **Upgrade all packages:**
  ```bash
  brew upgrade
  ```

- **Upgrade a specific package:**
  ```bash
  brew upgrade <package-name>
  ```

- **Search for a package:**
  ```bash
  brew search <package-name>
  ```

- **List installed packages:**
  ```bash
  brew list
  ```

- **Check for outdated packages:**
  ```bash
  brew outdated
  ```


## Exporting and Installing Packages

This command saves all your installed formulae and casks to the specified `Brewfile` in your dotfiles.

```bash
brew bundle dump --file=~/repos/dotfiles/install/hosts/macOS/Brewfile --force
```

To install all packages and apps from your `Brewfile` on a new system:

```bash
brew bundle --file=~/repos/dotfiles/install/hosts/macOS/Brewfile
```


## Link dotfiles
```
bash install/hosts/scripts/link_config.sh install/hosts/macOS/mac02.conf
```

# macinstall

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
brew bundle dump --file=~/repos/dotfiles/install/macinstall/Brewfile --force
```

To install all packages and apps from your `Brewfile` on a new system:

```bash
brew bundle --file=~/repos/dotfiles/install/macinstall/Brewfile
```


## Link dotfiles
```
./scripts/link_config.sh install/macinstall/macos_links.conf
```

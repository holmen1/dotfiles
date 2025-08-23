# macinstall


## Exporting and Installing Packages

```bash
brew bundle dump --file=~/repos/dotfiles/install/macinstall/Brewfile --force
```
This command saves all your installed formulae and casks to the specified `Brewfile` in your dotfiles.

To install all packages and apps from your `Brewfile` on a new system:

```bash
brew bundle --file=~/repos/dotfiles/install/macinstall/Brewfile
```


## Link dotfiles
```
./scripts/link_config.sh install/macinstall/macos_links.conf
```

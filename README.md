# dotfiles


## Automation scripts
```
curl -O https://raw.githubusercontent.com/holmen1/dotfiles/refs/heads/master/scripts/gitconfig.sh
curl -O https://raw.githubusercontent.com/holmen1/dotfiles/refs/heads/master/scripts/generate_key.sh

chmod +x gitconfig.sh generate_key.sh
```

* configure Git
```
./gitconfig.sh
```
* Generate SSH key
```
./generate_key.sh
```
After you generate an SSH key pair, you must add the public key to GitHub.com to enable SSH access for your account


## Xmonad
xmonad is a tiling window manager for X. Windows are arranged automatically to tile the screen without gaps or overlap, maximizing screen use, here configured with xmobar to provide a status bar  

[REAME](https://github.com/holmen1/dotfiles/blob/master/dotfiles/xmonad/README.md)

## kitty
[README](https://github.com/holmen1/dotfiles/blob/master/dotfiles/kitty/README.md)

## neovim
[README](https://github.com/holmen1/dotfiles/blob/master/dotfiles/nvim/README.md)



# LESSONS LEARNED

## Debug
~/.xsession-errors

## Display manager
```
sudo systemctl disable lightdm
```

Start xmonad with: startx  
start xfce: startx /usr/bin/startxfce4

## Opacity
Need xcompmgr for fadeWindowsLogHook to work

## tty
C-A-F2, then startx

## VSCode navigation
Focus the Terminal:

Ctrl + ` (backtick): Toggles the terminal visibility and focuses on it  
Ctrl + Shift + `: Creates a new terminal and focuses on it  

Focus Back to the Editor:

Ctrl + 1: Focus on the first editor group  
Ctrl + 2, Ctrl + 3, etc., for other editor groups

Explorer:  
Ctrl + Shift + E: Focus on the Explorer view

## lspconfig
```
yay -S unzip
```
luastyles: failed to install



## TODO

### Install
* [ ] Network manager
* [ ] Neovim
* [ ] Install script



### Deskstop
sudo tasksel

### Python
sudo apt install python3.11-venv

### Vim
sudo apt remove vim-tiny  
sudo apt install vim  

.vimrc  
" Turn on syntax highlighting  
syntax on  

" Show line numbers  
set number  

" Encoding  
set encoding=utf-8  

" Status bar  
set laststatus=2  

" Last line  
set showmode  
set showcmd  

### VScode
sudo apt install ./Downloads/code_1.92.0-1722473020_amd64.deb

### VScode (from Flatpack) run from terminal [not recomended]
sudo ln -s /var/lib/flatpak/exports/bin/com.visualstudio.code /usr/local/bin/code  

### dconf
sudo dconf-editor /

### Cleanup
sudo apt autoremove  
sudo apt clean  
sudo flatpak uninstall --unused


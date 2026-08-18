# LF

`lf` (as in "list files") is a terminal file manager written in Go with a heavy inspiration from `ranger` file manager

[Wiki](https://github.com/gokcehan/lf/wiki)


## Keymaps

### Default Keymaps

| Command | Description |
|---------|-------------|
| `gg` / `G`   | Go to top/bottom of list    |
| `/`          | Search                      |
| `Enter`      | Open file with default opener|
| `y`          | Yank (copy) file            |
| `p`          | Paste file                  |
| `r`          | Rename file                 |
| `a`          | mkdir                       |
| `q`          | Quit                        |
| `zh`         | Toggle hidden files         |
| `gh`         | cd ~                        |

### Custom Keymaps

| Command      | Description                 |
|--------------|-----------------------------|
| `ze`         | Extract                     |
| `zc`         | Compress with tar and gunzip|
| `zd`         | Move to .trash              |
| `gt`         | Open terminal               |
| `x`          | Execute file in subshell    |
| `X`          | Execute file in background  |
| ` (backtick) | Result of previous command  |

## Image Preview

Image preview is configured using `feh` as the previewer. The `preview.sh` script handles different file types:
- Image files are displayed using `feh --scale-down`
- Text files show a preview of their content
- Other files show their file type information






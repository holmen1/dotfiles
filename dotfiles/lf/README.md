# LF

`lf` (as in "list files") is a terminal file manager written in Go with a heavy inspiration from `ranger` file manager

[Wiki](https://github.com/gokcehan/lf/wiki)


## Keymaps

### Default Keymaps

| Command | Description |
|---------|-------------|
| `gg` / `G` | Go to top/bottom of list |
| `/` | Search |
| `Enter` | Open file with default opener |
| `d` | Move to .trash |
| `y` | Yank (copy) file |
| `p` | Paste file |
| `r` | Rename file |
| `a` | mkdir |
| `q` | Quit |
| `zh` | Toggle hidden files |
| `z?` | Show help |
| `gh` | cd ~ |

### Custom Keymaps

| Command | Description |
|---------|-------------|
| `x` | Execute current file in a subshell |
| `X` | Execute current file in background |
| ` (backtick) | Show result of previous command |
| `:tar` | Compress current file or selected files with tar and gunzip |
| `:untar` | Extract current file with the right command |

## Image Preview

Image preview is configured using `feh` as the previewer. The `preview.sh` script handles different file types:
- Image files are displayed using `feh --scale-down`
- Text files show a preview of their content
- Other files show their file type information






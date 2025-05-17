# .

## Add to your .bashrc:

```bash
# Enable vi mode in bash
set -o vi

# Show current mode in prompt (optional)
bind 'set show-mode-in-prompt on'

# Make Tab autocomplete regardless of filename case
bind 'set completion-ignore-case on'

# Make history searching work like vim
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
```

## Basic vi mode navigation:

- Press `Esc` to enter command mode
- `k` to navigate up through history
- `j` to navigate down through history
- `/` to search history
- `n` to find next match
- `N` to find previous match

This approach leverages bash's built-in capabilities and works on any terminal, making it more "suckless" than relying on terminal-specific patches.
# neovim

Windows

:vs [file_path] - split the screen vertically  
:sp [file_path] - split the window horizontally  
Ctrl + w and press h - move to the split window on the left  
Ctrl + w and press j - move to the split window on the down  
Ctrl + w and press k - move to the split window on the up  
Ctrl + w and press l - move to the split window on the right

## Custom keymap

### neo-tree
leader nt - open neotree  
a - create a new file OR directory. Add a `/` to the end of the name to make a directory  
d - delete a file or directory  
i - show file details in popup window  
r - rename a file or directory
y - copy the selected file or directory  
p - paste the selected file or directory  
m - move the selected file or directory      

### gitsigns
[c - Jump to previous git change  
leader hs - stage hunk  
leader hr - reset hunk  
leader hS - stage buffer  
leader hu - undo stage hunk  
leader hR - reset buffer  
leader hp - preview hunk  
leader hb - blame line  
leader hd - diff against index

### LSP
K - vim.lsp.buf.hover  
leader gd - vim.lsp.buf.definition  
leader gr - vim.lsp.buf.references  
leader ca - vim.lsp.buf.code_action  

### Telescope
leader sf - builtin.find_files  
leader ss - builtin.builtin  
leader sw - builtin.grep_string  
leader sg - builtin.live_grep

### Small Terminal (Custom)
leader st - open terminal
C-\ C-n - exit terminal mode


## vim-cheat

w - jump forwards to the start of a word

b - jump backwards to the start of a word

e - jump forwards to the end of a word

0 - jump to the start of the line

$ - jump to the end of the line

gg - go to the first line of the document

G - go to the last line of the document

5gg - go to line 5



Search and replace

/pattern - search for pattern

n - repeat search in same direction

N - repeat search in opposite direction

Ctrl + o - go back to where you came from

% - move cursor to matching character (default supported pairs: '()', '{}', '[]')

:s/old/new - replace first occurrence of old

:s/old/new/g - replace  substitute globally in the line

:#,#s/old/new/g - where #,# are the line numbers of the range of lines

:%s/old/new/g - replace all old with new throughout file

:%s/old/new/gc - replace all old with new throughout file with confirmations



I - insert at the beginning of the line

a - insert (append) after the cursor

A - insert (append) at the end of the line

o - append (open) a new line below the current line

O - append (open) a new line above the current line



Editing

r - replace a single character

R - replace more than one character

cc - change (replace) entire line

ciw - change (replace) entire word

cw or ce - change (replace) to the end of the word

u - undo

U - restore (undo) last changed line



Cut and paste

yy - yank (copy) a line

2yy - yank (copy) 2 lines

yw - yank (copy) the characters of the word from the cursor position to the start of the next word

dd - delete (cut) a line

2dd - delete (cut) 2 lines

dw - delete (cut) the characters of the word from the cursor position to the start of the next word

d$ or D - delete (cut) to the end of the line

p - put (paste) the clipboard after cursor

P - put (paste) before cursor



Write and read files

v motion :w FILENAME - saves the Visually selected lines in file FILENAME

:r FILENAME - retrieves disk file FILENAME and puts it below the cursor position






## Nerd-fonts
```
$ sudo pacman -S nerd-fonts
```
select 42) ttf-jetbrains-mono-nerd  
Add font to terminal profile


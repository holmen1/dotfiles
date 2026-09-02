# Terminal Color Schemes — st · dmenu · lf · bash

Three coordinated, comfortable schemes for an ascetic Linux workstation
(**st** terminal, **lf** file manager, **dmenu**, **bash**). No `st`
`config.h` edits required — everything is driven with 256-color / truecolor
escapes plus `OSC 10`, which `st` renders directly.

Each scheme shares **one palette** across bash, dmenu, and lf. lf inherits
the `LS_COLORS` value exported by bash, so a role (e.g. "directory") looks
identical everywhere it appears.

## Schemes at a glance

| Scheme    | Mood                              | Accent      | Background |
|-----------|-----------------------------------|-------------|------------|
| **Amber** | Retro warm CRT, red↔yellow spread | `#d9a441`   | `#1c1c1c`  |
| **Ash**   | Cool neutral gray/teal            | `#5f9ea0`   | `#1c1c1c`  |
| **Ember** | Warm amber + deep red             | `#e0913a`   | `#1c1c1c`  |

Notes shared by all schemes:
- **dmenu** uses only two colors (background + accent), selection inverted —
  exactly as the current config.
- **lf** inherits `LS_COLORS` from `.bashrc`. `LF_COLORS` and
  `~/.config/lf/colors` override it, so leave both unset when `.bashrc` is
  the source of truth.
- **Prompt**: `user@host` cool gray, path tan, git branch warm, and a `$`
  terminator that turns **red on a non-zero exit status**.

---

## How to apply

All color settings live in `config/bash/.bashrc` (linked as `~/.bashrc`).
Paste the chosen scheme's bash block there, replacing the existing
`COLOR_*`, `LS_COLORS`, `GREP_COLORS`, `OSC 10`, `DMENU_COLORS`,
`DMENU_HELP_COLORS`, and `PS1` settings. dmenu consumes the exported
`DMENU_*` values and lf consumes the exported `LS_COLORS` value.

### Prompt exit-status snippet (shared by all schemes)

`PS1` cannot read `$?` reliably inline, so snapshot it first:

```bash
# Put this ONCE, above the PS1 line. It preserves any existing $? for the prompt.
__prompt_ec() { __ec=$?; }
PROMPT_COMMAND="__prompt_ec${PROMPT_COMMAND:+; $PROMPT_COMMAND}"
```

Then each scheme's `PS1` chooses the `$` color from `$__ec` (see per-scheme
`COLOR_PROMPT_OK` / `COLOR_PROMPT_ERR`).

---

## Shared role → color map

The same role uses the same value in terminal output, lf, and (where the two
dmenu slots apply) the menu.

| Role            | Amber              | Ash                | Ember              |
|-----------------|--------------------|--------------------|--------------------|
| Background      | `#1c1c1c` / 234    | `#1c1c1c` / 234    | `#1c1c1c` / 234    |
| Foreground      | `#e8cfa8` / 187    | `#c6c6c6` / 251    | `#e8cfa8` / 187    |
| Accent          | `#d9a441` / 179    | `#5f9ea0` / 73     | `#e0913a` / 173    |
| Directory       | `#d9a441` / 179    | `#5f9ea0` / 73     | `#e0913a` / 173    |
| Symlink (ln)    | `#e0c060` / 185    | `#87afaf` / 109    | `#d7b060` / 179    |
| Executable (ex) | `#cc7a33` / 173    | `#5f87af` / 67     | `#c0562e` / 166    |
| Orphan (or)     | red `31;01`        | red `31;01`        | red `31;01`        |
| Prompt `$` OK   | `#d9a441` / 179    | `#5f9ea0` / 73     | `#e0913a` / 173    |
| Prompt `$` ERR  | `#b0503a` / 130    | `#af5f5f` / 131    | `#a23b2a` / 124    |

---

# 1 · Amber

Retro warm CRT feel — but *not* monochrome. Distinction comes from a purely
warm red↔yellow spread (no green): gold dirs, burnt-orange executables, pale
amber links, deep-red archives, dusty-rose media, muted-ochre audio.

### bash

```bash
# Color definitions (prompt uses \[...\] zero-width escapes for readline)
export COLOR_RESET='\[\e[0m\]'
export COLOR_USER='\[\e[38;5;243m\]'        # cool gray — user@host recedes
export COLOR_PATH='\[\e[38;5;180m\]'        # tan — readable, humble
export COLOR_GIT='\[\e[38;5;137m\]'         # warm ochre — branch
export COLOR_PROMPT_OK='\[\e[38;5;179m\]'   # gold amber — $ when last exit 0
export COLOR_PROMPT_ERR='\[\e[38;5;130m\]'  # deep red — $ when last exit != 0

export LS_COLORS='di=38;5;179:ln=38;5;185:ex=38;5;173:fi=38;5;187:or=31;01:*.sh=38;5;173:*.py=38;5;179:*.js=38;5;185'
export GREP_COLORS='ms=38;5;179:fn=38;5;180:ln=38;5;137'

# Default (unstyled) text tint via OSC 10 — soft amber-on-dark, CRT-like
printf '\e]10;#e8cfa8\a'
printf '\e]11;#1c1c1c\a'   # background (optional; harmless if st ignores)

export DMENU_COLORS="-nb #1c1c1c -nf #d9a441 -sb #d9a441 -sf #1c1c1c"
export DMENU_HELP_COLORS="-nb #1c1c1c -nf #d9a441 -sb #1c1c1c -sf #d9a441"

# Exit-aware $ prompt (needs the shared __prompt_ec snippet above)
export PS1="${COLOR_USER}\u@\h ${COLOR_PATH}\W${COLOR_GIT}\$(__git_ps1 ' (%s)')\[\e[38;5;\$([ \"\$__ec\" = 0 ] && echo 179 || echo 130)m\]\$ ${COLOR_RESET}"
```

---

# 2 · Ash

Cool neutral gray with a teal accent — calm, low-fatigue, good for long
sessions. Same structure, cool hues.

### bash

```bash
export COLOR_RESET='\[\e[0m\]'
export COLOR_USER='\[\e[38;5;245m\]'        # gray
export COLOR_PATH='\[\e[38;5;109m\]'        # muted teal-gray path
export COLOR_GIT='\[\e[38;5;108m\]'         # sage
export COLOR_PROMPT_OK='\[\e[38;5;73m\]'    # teal — $ ok
export COLOR_PROMPT_ERR='\[\e[38;5;131m\]'  # dusty red — $ error

export LS_COLORS='di=38;5;73:ln=38;5;109:ex=38;5;67:fi=38;5;251:or=31;01:*.sh=38;5;67:*.py=38;5;73:*.js=38;5;109'
export GREP_COLORS='ms=38;5;73:fn=38;5;109:ln=38;5;108'

printf '\e]10;#c6c6c6\a'
printf '\e]11;#1c1c1c\a'

export DMENU_COLORS="-nb #1c1c1c -nf #5f9ea0 -sb #5f9ea0 -sf #1c1c1c"
export DMENU_HELP_COLORS="-nb #1c1c1c -nf #5f9ea0 -sb #1c1c1c -sf #5f9ea0"

export PS1="${COLOR_USER}\u@\h ${COLOR_PATH}\W${COLOR_GIT}\$(__git_ps1 ' (%s)')\[\e[38;5;\$([ \"\$__ec\" = 0 ] && echo 73 || echo 131)m\]\$ ${COLOR_RESET}"
```

---

# 3 · Ember

Warmest of the three — amber accent with deep-red weight. A touch more
saturated than Amber, still no green.

### bash

```bash
export COLOR_RESET='\[\e[0m\]'
export COLOR_USER='\[\e[38;5;243m\]'
export COLOR_PATH='\[\e[38;5;180m\]'
export COLOR_GIT='\[\e[38;5;173m\]'
export COLOR_PROMPT_OK='\[\e[38;5;173m\]'   # amber — $ ok
export COLOR_PROMPT_ERR='\[\e[38;5;124m\]'  # deep red — $ error

export LS_COLORS='di=38;5;173:ln=38;5;179:ex=38;5;166:fi=38;5;187:or=31;01:*.sh=38;5;166:*.py=38;5;173:*.js=38;5;179'
export GREP_COLORS='ms=38;5;173:fn=38;5;180:ln=38;5;179'

printf '\e]10;#e8cfa8\a'
printf '\e]11;#1c1c1c\a'

export DMENU_COLORS="-nb #1c1c1c -nf #e0913a -sb #e0913a -sf #1c1c1c"
export DMENU_HELP_COLORS="-nb #1c1c1c -nf #e0913a -sb #1c1c1c -sf #e0913a"

export PS1="${COLOR_USER}\u@\h ${COLOR_PATH}\W${COLOR_GIT}\$(__git_ps1 ' (%s)')\[\e[38;5;\$([ \"\$__ec\" = 0 ] && echo 173 || echo 124)m\]\$ ${COLOR_RESET}"
```

---

## Verifying colors

Quick 256-color reference to eyeball any code `N`:

```sh
for i in 179 173 185 187 130 174 137 73 67 109 251 103 108 166 124 132; do
  printf '\e[38;5;%dm %3d ██\e[0m\n' "$i" "$i"
done
```

Restart lf after changing `LS_COLORS` in `.bashrc`.
`.bashrc` changes apply in new shells or after `source ~/.bashrc` (the
`OSC 10/11` lines re-tint the running terminal immediately).

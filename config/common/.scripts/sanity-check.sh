#!/bin/sh

DOTFILES_DIR=$HOME/repos/dotfiles
. "$DOTFILES_DIR/install/hosts/scripts/host-profile.sh"
if ! select_host; then
    echo "Error: sanity checks are not supported on this OS or distribution" >&2
    exit 1
fi
load_host_profile || exit 1
if [ -z "${terminal-}" ] || [ -z "${window_manager-}" ] || [ "${packages+x}" != x ]; then
    echo "Error: invalid host configuration '$HOST_CONFIG'" >&2
    exit 1
fi

passed=0
failed=0
warnings=0

ok()   { printf '[ok] %s\n' "$1"; passed=$((passed + 1)); }
fail() { printf '[FAIL] %s\n' "$1"; failed=$((failed + 1)); }
warn() { printf '[warn] %s\n' "$1"; warnings=$((warnings + 1)); }

linked() {
    case " $packages " in
        *" $1 "*) return 0 ;;
        *) return 1 ;;
    esac
}

check_command() {
    if command -v "$1" >/dev/null 2>&1; then
        ok "$1"
    else
        fail "$1 not found"
    fi
}

check_file() {
    if [ -f "$1" ]; then
        ok "$1"
    else
        fail "$1 not found"
    fi
}

check_link() {
    if [ -L "$1" ] && [ -e "$1" ]; then
        ok "$1"
    else
        fail "$1 is not a valid symlink"
    fi
}

printf 'Sanity check: %s (%s)\n' "$HOSTNAME" "$OS"
check_command "$terminal"
check_command "$window_manager"

if linked common; then
    check_link "$HOME/.xinitrc"
    check_command git
    check_command ssh
    check_command dmenu-menu
    if git config --global user.name >/dev/null 2>&1 &&
       git config --global user.email >/dev/null 2>&1; then
        ok "Git identity configured"
    else
        warn "Git identity not configured"
    fi
    if [ -f "$HOME/.ssh/id_ed25519" ] && [ -f "$HOME/.ssh/id_ed25519.pub" ]; then
        ok "SSH key exists"
    else
        warn "SSH key not found"
    fi
fi

if linked bsdinstall || linked artixinstall; then
    check_command startx
    check_command xbindkeys
    check_link "$HOME/.xbindkeysrc"
fi

if linked artixinstall; then
    check_command Xorg
    for service in iwd dbus elogind; do
        if rc-service "$service" status >/dev/null 2>&1; then
            ok "$service running"
        else
            warn "$service not running"
        fi
    done
fi

if linked xmonad; then
    check_file "$HOME/.config/xmonad/xmonad.hs"
fi

if linked bash; then
    check_command bash
    check_link "$HOME/.bashrc"
    if [ "$OS" = FreeBSD ]; then
        expected_shell=/usr/local/bin/bash
        user_entry=$(pw usershow "$(id -un)" -7) || user_entry=
    else
        expected_shell=bash
        user_entry=$(getent passwd "$(id -un)") || user_entry=
    fi
    if [ -n "$user_entry" ]; then
        user_shell=$(printf '%s\n' "$user_entry" | awk -F: '{print $NF}')
        case "$OS:$user_shell" in
            FreeBSD:/usr/local/bin/bash|Linux:/bin/bash|Linux:/usr/bin/bash)
                ok "Bash is the configured login shell" ;;
            *) fail "Login shell is '$user_shell', expected $expected_shell" ;;
        esac
    else
        fail "Could not check the login shell"
    fi
fi

if linked nvim; then
    check_command nvim
    check_file "$HOME/.config/nvim/init.lua"
fi

if linked xkb; then
    check_command xkb-toggle
    if [ -f "$HOME/.cache/custom-keymap.xkb" ]; then
        ok "Custom XKB keymap"
    else
        warn "Custom XKB keymap not found"
    fi
fi

if linked lf; then
    check_command lf
    check_file "$HOME/.config/lf/lfrc"
fi

printf '\nPassed: %s  Failed: %s  Warnings: %s\n' "$passed" "$failed" "$warnings"
[ "$failed" -eq 0 ]

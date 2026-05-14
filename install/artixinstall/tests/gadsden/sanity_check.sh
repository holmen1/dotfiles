#!/bin/sh

PASS=0
FAIL=0

ok()   { printf "  [ok] %s\n" "$1"; PASS=$((PASS+1)); }
fail() { printf " [!!] %s\n" "$1"; FAIL=$((FAIL+1)); }
warn() { printf "  [?] %s\n" "$1"; }
hdr()  { printf "\n--- %s\n" "$1"; }

check_cmd()     { command -v "$1" >/dev/null 2>&1 && ok "$1" || fail "$1 not found"; }
check_file()    { [ -f "$1" ] && ok "$1" || fail "$1 not found"; }
check_symlink() {
    if [ -L "$1" ] && [ -e "$1" ]; then ok "$1 -> $(readlink "$1")"
    elif [ -L "$1" ];               then fail "$1 (broken symlink)"
    else                                 fail "$1 (not a symlink)"
    fi
}
check_service() {
    if rc-service "$1" status >/dev/null 2>&1; then
        ok "$1 running"
    else
        fail "$1 not running"
    fi
}

printf "Sanity check — gadsden (artix/openrc)\n"

hdr "Core commands"
check_cmd git
check_cmd ssh
check_cmd startx
check_cmd xmonad
check_cmd xterm
check_cmd st
check_cmd dmenu
check_cmd nvim

hdr "X session"
check_file "$HOME/.xinitrc"
if [ -x /usr/local/bin/xmonad ]; then ok "/usr/local/bin/xmonad"
else fail "/usr/local/bin/xmonad not found or not executable"; fi
check_cmd xbindkeys
check_cmd scrot
check_cmd i3lock

hdr "Notifications"
check_cmd xmessage

hdr "OpenRC services"
check_service dbus
check_service dhcpcd
check_service elogind
check_service iwd

hdr "Dotfile symlinks"
check_symlink "$HOME/.config/nvim"
check_symlink "$HOME/.xinitrc"

hdr "Git"
git_name=$(git config --global user.name 2>/dev/null)
git_email=$(git config --global user.email 2>/dev/null)
[ -n "$git_name" ]  && ok "user.name: $git_name"  || fail "git user.name not set"
[ -n "$git_email" ] && ok "user.email: $git_email" || fail "git user.email not set"

hdr "SSH"
if [ -f "$HOME/.ssh/id_ed25519" ]; then
    ok "id_ed25519 exists"
    perms=$(stat -c %a "$HOME/.ssh/id_ed25519" 2>/dev/null)
    [ "$perms" = "600" ] && ok "id_ed25519 perms 600" || fail "id_ed25519 perms $perms (want 600)"
else
    fail "~/.ssh/id_ed25519 not found"
fi

hdr "wheel group"
groups | grep -q wheel && ok "user in wheel group" || fail "user not in wheel group"
sudo -n true 2>/dev/null && ok "passwordless sudo enabled" || warn "passwordless sudo not configured"

printf "\nPassed: %d  Failed: %d\n" "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ] && exit 0 || exit 1

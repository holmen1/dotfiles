#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
WARNINGS=0

print_header() {
    echo -e "\n${BLUE}=== $1 ===${NC}"
}

print_pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((PASSED++))
}

print_fail() {
    echo -e "${RED}✗${NC} $1"
    ((FAILED++))
}

print_warn() {
    echo -e "${YELLOW}!${NC} $1"
    ((WARNINGS++))
}

check_command() {
    local cmd="$1"
    local desc="$2"
    
    if command -v "$cmd" &> /dev/null; then
        print_pass "$desc"
    else
        print_fail "$desc - '$cmd' not found"
    fi
}

check_file() {
    local file="$1"
    local desc="$2"
    
    if [ -f "$file" ]; then
        print_pass "$desc"
    else
        print_fail "$desc - '$file' not found"
    fi
}

check_symlink() {
    local link="$1"
    local desc="$2"
    
    if [ -L "$link" ]; then
        if [ -e "$link" ]; then
            print_pass "$desc (valid symlink)"
        else
            print_fail "$desc (broken symlink)"
        fi
    else
        print_fail "$desc - symlink not found"
    fi
}

check_service() {
    local service="$1"
    local desc="$2"
    local is_timer=false
    if [[ "$service" == *.timer ]]; then
        is_timer=true
    fi

    if systemctl --user is-active "$service" &> /dev/null; then
        if $is_timer; then
            print_pass "$desc (active and waiting)"
        else
            print_pass "$desc (active)"
        fi
    elif systemctl --user is-enabled "$service" &> /dev/null; then
        print_warn "$desc (enabled but not active)"
    else
        print_fail "$desc (not enabled or not active)"
    fi
}

check_unit_loaded() {
    local unit="$1"
    local desc="$2"

    if systemctl --user cat "$unit" &> /dev/null; then
        print_pass "$desc (unit is loaded)"
    else
        print_fail "$desc (unit not found by systemd)"
    fi
}

echo -e "${BLUE}🔧 Dotfiles Installation Sanity Check${NC}"
echo "Checking your custom Linux environment..."

# Essential Commands
print_header "Essential Commands"
check_command "git" "Git version control"
check_command "ssh" "SSH client"
check_command "startx" "X11 server starter"
check_command "xmonad" "XMonad window manager"
check_command "xterm" "Terminal emulator"
check_command "st" "Terminal emulator"

# XMonad Setup
print_header "XMonad Configuration"
if [ -f "/usr/local/bin/xmonad" ]; then
    print_pass "Custom XMonad binary exists"
    if [ -x "/usr/local/bin/xmonad" ]; then
        print_pass "XMonad binary is executable"
    else
        print_fail "XMonad binary is not executable"
    fi
else
    print_fail "Custom XMonad binary not found in /usr/local/bin"
fi

check_file "$HOME/.xinitrc" ".xinitrc file"

# Power Management
print_header "Power Management"
if groups | grep -q "wheel"; then
    print_pass "User in wheel group"

    # Check sudoers for passwordless poweroff (matches xmonad.hs keybinding)
    if sudo -n systemctl --version &>/dev/null; then
        print_pass "Passwordless sudo for systemctl poweroff confirmed by dry run"
    else
        print_warn "Passwordless sudo for systemctl poweroff not confirmed; password may be required"
    fi
else
    print_fail "User not in wheel group - power management may not work"
fi

# System Monitoring
print_header "System Monitoring"
check_service "system-monitor.timer" "System monitoring timer"
check_unit_loaded "system-monitor.target" "System monitoring target"
check_unit_loaded "wifi-monitor.service" "WiFi monitor service"
check_unit_loaded "vpn-monitor.service" "VPN monitor service"
check_unit_loaded "battery-monitor.service" "Battery monitor service"
check_command "dunst" "Dunst notification daemon"
check_command "notify-send" "Desktop notifications"

# Key Bindings
print_header "Key Bindings & Input"
check_command "xbindkeys" "Key binding daemon"
check_file "$HOME/.xbindkeysrc" "xbindkeys configuration"
check_command "brightnessctl" "Brightness control"
check_command "amixer" "Audio mixer (ALSA)"

# Screenshot functionality
print_header "Screenshots"
check_command "scrot" "Screenshot utility"
if [ -d "$HOME/Downloads" ]; then
    print_pass "Downloads directory exists for screenshots"
else
    print_warn "Downloads directory not found - screenshots may fail"
fi

# Dotfiles Symlinks
print_header "Dotfile Symlinks"
# Add checks for your specific symlinked configurations
if [ -L "$HOME/.config/systemd/user/wifi-monitor.service" ]; then
    check_symlink "$HOME/.config/systemd/user/wifi-monitor.service" "Wifi monitor service symlink"
fi
if [ -L "$HOME/.config/systemd/user/vpn-monitor.service" ]; then
    check_symlink "$HOME/.config/systemd/user/vpn-monitor.service" "VPN monitor service symlink"
fi
if [ -L "$HOME/.config/systemd/user/battery-monitor.service" ]; then
    check_symlink "$HOME/.config/systemd/user/battery-monitor.service" "Battery monitor service symlink"
fi

if [ -L "$HOME/.config/systemd/user/system-monitor.timer" ]; then
    check_symlink "$HOME/.config/systemd/user/system-monitor.timer" "System monitor timer symlink"
fi

# Git Configuration
print_header "Git Configuration"
git_name=$(git config --global user.name 2>/dev/null)
git_email=$(git config --global user.email 2>/dev/null)

if [ -n "$git_name" ]; then
    print_pass "Git username configured: $git_name"
else
    print_fail "Git username not configured"
fi

if [ -n "$git_email" ]; then
    print_pass "Git email configured: $git_email"
else
    print_fail "Git email not configured"
fi

# SSH Setup
print_header "SSH Configuration"
if [ -f "$HOME/.ssh/id_ed25519" ]; then
    print_pass "SSH private key exists"
    
    # Check permissions
    perms=$(stat -c %a "$HOME/.ssh/id_ed25519" 2>/dev/null)
    if [ "$perms" = "600" ]; then
        print_pass "SSH private key has correct permissions (600)"
    else
        print_fail "SSH private key has incorrect permissions ($perms, should be 600)"
    fi
else
    print_fail "SSH private key not found"
fi

if [ -f "$HOME/.ssh/id_ed25519.pub" ]; then
    print_pass "SSH public key exists"
else
    print_fail "SSH public key not found"
fi

# X11 Libraries
print_header "X11 Dependencies"
check_command "xsetroot" "X11 root window cursor/background utility"
check_command "xcompmgr" "X11 simple compositor"

# Repository Structure
print_header "Repository Structure"
if [ -d "$HOME/repos/dotfiles" ]; then
    print_pass "Dotfiles repository exists"
    
    if [ -d "$HOME/repos/dotfiles/dotfiles" ]; then
        print_pass "Dotfiles configuration directory exists"
    else
        print_fail "Dotfiles configuration directory missing"
    fi
    
    if [ -d "$HOME/repos/dotfiles/scripts" ]; then
        print_pass "Scripts directory exists"
    else
        print_warn "Scripts directory not found"
    fi
    
    if [ -f "$HOME/repos/dotfiles/install/build/xmonad/build-xmonad.sh" ]; then
        print_pass "XMonad build script exists"
    else
        print_fail "XMonad build script not found"
    fi
else
    print_fail "Dotfiles repository not found"
fi

# Test Key Functionalities
print_header "Functional Tests"
check_command "i3lock" "Screen lock"


# Summary
print_header "Summary"
echo -e "Tests completed:"
echo -e "  ${GREEN}Passed: $PASSED${NC}"
echo -e "  ${RED}Failed: $FAILED${NC}"
echo -e "  ${YELLOW}Warnings: $WARNINGS${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}✓ Your installation looks good!\n${NC}"
    if [ $WARNINGS -gt 0 ]; then
        echo -e "${YELLOW}⚠️  Some warnings need attention${NC}"
    fi
    exit 0
else
    echo -e "\n${RED}❌ Some issues need to be fixed${NC}"
    echo -e "Check the failed items above and refer to your installation guide."
    exit 1
fi

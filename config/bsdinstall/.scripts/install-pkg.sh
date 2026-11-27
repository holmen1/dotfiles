#!/bin/sh
# FreeBSD
# Install packages from a pkglist or host profile.

usage() {
    echo "Usage: $0 --profile <host-config>" >&2
    exit 1
}

install_packages() {
    while IFS= read -r package || [ -n "$package" ]; do
        case "$package" in
            *[![:space:]]*) ;;
            *) continue ;;
        esac
        if ! pkg query '%n' "$package" >/dev/null 2>&1; then
            echo "Installing $package..."
            sudo pkg install -y "$package" || return 1
        else
            echo "$package is already installed, skipping."
        fi
    done
}

if [ "${1-}" = --profile ]; then
    [ "$#" -eq 3 ] || usage
    [ -f "$2" ] || { echo "Error: $2 not found" >&2; exit 1; }
    profile=$2
    case "$profile" in
        */*) ;;
        *) profile=./$profile ;;
    esac
    unset pkg_packages
    . "$profile"
    if [ "${pkg_packages+x}" != x ]; then
        echo "Error: native package list missing from '$2'" >&2
        exit 1
    fi
    printf '%s\n' "$pkg_packages" | install_packages
else
    "Error: " >&2; exit 1;
fi

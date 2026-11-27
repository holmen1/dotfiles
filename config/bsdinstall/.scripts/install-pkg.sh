#!/bin/sh
# FreeBSD
# Install packages from a pkglist or host profile.

usage() {
    echo "Usage: $0 --profile <host-config> [--pre|--packages|--foreign]" >&2
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

[ "$#" -ge 2 ] && [ "$#" -le 3 ] && [ "$1" = --profile ] || usage
case "${3---packages}" in
    --pre) group=pkg_pre ;;
    --packages) group=pkg_packages ;;
    --foreign) group=foreign_packages ;;
    *) usage ;;
esac

[ -f "$2" ] || { echo "Error: $2 not found" >&2; exit 1; }
profile=$2
case "$profile" in
    */*) ;;
    *) profile=./$profile ;;
esac
unset pkg_pre pkg_packages foreign_packages
. "$profile" || exit 1
case "$group" in
    pkg_pre)
        [ "${pkg_pre+x}" = x ] || { echo "Error: pkg_pre missing from '$2'" >&2; exit 1; }
        package_list=$pkg_pre
        ;;
    pkg_packages)
        [ "${pkg_packages+x}" = x ] || { echo "Error: pkg_packages missing from '$2'" >&2; exit 1; }
        package_list=$pkg_packages
        ;;
    foreign_packages)
        [ "${foreign_packages+x}" = x ] || { echo "Error: foreign_packages missing from '$2'" >&2; exit 1; }
        package_list=$foreign_packages
        ;;
esac
printf '%s\n' "$package_list" | install_packages

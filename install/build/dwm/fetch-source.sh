#!/bin/sh
set -e

# Configuration
DWM_VERSION="6.8"
BUILD_DIR="${HOME}/repos/dotfiles/install/build/dwm"
PATCH_DIR="${BUILD_DIR}/patches"

PATCH_AUTOSTARTTAGS_VERSION="6.4"

# Download source
cd "${BUILD_DIR}"
echo "Downloading dwm ${DWM_VERSION}..."
curl -L "https://dl.suckless.org/dwm/dwm-${DWM_VERSION}.tar.gz" | tar xz

# Download patches
mkdir -p "${PATCH_DIR}"
cd "${PATCH_DIR}"
echo "Downloading patches..."

# Allow dwm to execute commands on specific tags during startup 
curl -O "https://dwm.suckless.org/patches/autostarttags/dwm-autostarttags-${PATCH_AUTOSTARTTAGS_VERSION}.diff"


#!/bin/bash
set -e

# Configuration
PATCH_FONT2_VERSION="0.8.5"
PATCH_ALPHA_VERSION="20220206-0.8.5"
PATCH_SCROLLBACK_VERSION="0.9.2"
PATCH_DIR="${HOME}/repos/dotfiles/install/build/st/patches"

cd "${PATCH_DIR}"

echo "Downloading patches..."

# font2 allows to add spare font besides default
curl -O "https://st.suckless.org/patches/font2/st-font2-${PATCH_FONT2_VERSION}.diff"

# Alpha (transparency) patch
curl -O "https://st.suckless.org/patches/alpha/st-alpha-${PATCH_ALPHA_VERSION}.diff"

# Scrollback patch
curl -O "https://st.suckless.org/patches/scrollback/st-scrollback-${PATCH_SCROLLBACK_VERSION}.diff"

#!/usr/bin/env bash
set -e

# Configuration
ST_VERSION="0.9.3"
BUILD_DIR="${HOME}/repos/dotfiles/install/build/st"
PATCH_DIR="${BUILD_DIR}/patches"

PATCH_FONT2_VERSION="0.8.5"
PATCH_ALPHA_VERSION="20220206-0.8.5"
PATCH_SCROLLBACK_VERSION="0.9.2"
PATCH_SCROLLBACK_FLOAT_VERSION="float-0.9.2"

# Download source
cd "${BUILD_DIR}"
echo "Downloading st ${ST_VERSION}..."
curl -L "https://dl.suckless.org/st/st-${ST_VERSION}.tar.gz" | tar xz

# Download patches
mkdir -p "${PATCH_DIR}"
cd "${PATCH_DIR}"
echo "Downloading patches..."

# font2 allows to add spare font besides default
curl -O "https://st.suckless.org/patches/font2/st-font2-${PATCH_FONT2_VERSION}.diff"

# Alpha (transparency) patch
curl -O "https://st.suckless.org/patches/alpha/st-alpha-${PATCH_ALPHA_VERSION}.diff"

# Scrollback patches
curl -O "https://st.suckless.org/patches/scrollback/st-scrollback-${PATCH_SCROLLBACK_VERSION}.diff"
curl -O "https://st.suckless.org/patches/scrollback/st-scrollback-${PATCH_SCROLLBACK_FLOAT_VERSION}.diff"

#!/bin/sh
#
# Build/install xmonad + xmonad-contrib libraries using Cabal
#
# Prerequisites:
#   - GHC + cabal-install, check that versions are tested
#   - System C libraries: libX11, libXrandr, libXext, libXinerama, libXScrnSaver
#     Artix/Arch: pacman -S libx11 libxrandr libxext libxinerama libxss
#   - autoconf (for the X11 Haskell package)

set -e

XMONAD_VER="0.18.1"
XMONAD_CONTRIB_VER="0.18.2"

if command -v ghc >/dev/null 2>&1; then
    echo "Using $(ghc --version)"
else
    echo "Error: no ghc found on PATH"
    exit 1
fi

if command -v cabal >/dev/null 2>&1; then
    echo "and cabal-install version $(cabal --numeric-version)"
else
    echo "Error: no cabal found on PATH"
    exit 1
fi

echo ""
echo "=== Updating Cabal package index ==="
cabal update

echo ""
echo "=== Installing libraries into ~/.cabal/store ==="
# base ships as a boot/wired-in package with GHC itself
# no need separate install, and doing so can conflict with the compiler's install
cabal install \
    --force-reinstalls \
    --lib \
    xmonad-"${XMONAD_VER}" \
    xmonad-contrib-"${XMONAD_CONTRIB_VER}"

echo ""
echo "=== Done ==="
echo "Libraries are cached in the shared Cabal store - rerun this script"
echo "only when bumping xmonad/xmonad-contrib versions."

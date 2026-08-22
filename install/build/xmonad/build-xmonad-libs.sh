#!/bin/sh
#
# Build/install xmonad + xmonad-contrib libraries using Cabal, and write a
# GHC environment file so a later bare `ghc --make` can link against them
# without a generated .cabal project (see build-custom-xmonad.sh).
#
# Colocated with the real config (not the build factory) since this machine
# is both the build machine and the only target - xmonad --recompile (M-q)
# works for free as a side effect, though we don't rely on it.
#
# Prerequisites:
#   - GHC + cabal-install, check that versions are tested
#   - System C libraries: libX11, libXrandr, libXext, libXinerama, libXScrnSaver
#     Artix/Arch: pacman -S libx11 libxrandr libxext libxinerama libxss
#   - autoconf (for the X11 Haskell package)

set -e

XMONAD_VER="0.18.1"
XMONAD_CONTRIB_VER="0.18.2"

ENV_DIR=~/.config/xmonad

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

mkdir -p "$ENV_DIR"

echo ""
echo "=== Updating Cabal package index ==="
cabal update

echo ""
echo "=== Installing libraries into $ENV_DIR ==="
# base ships as a boot/wired-in package with GHC itself - no need to
# install it separately, and doing so can conflict with the compiler's
# own copy.
cabal install --lib --package-env="$ENV_DIR" \
    xmonad=="${XMONAD_VER}" \
    xmonad-contrib=="${XMONAD_CONTRIB_VER}"

echo ""
echo "=== Done ==="
echo "GHC environment file written to: $ENV_DIR/.ghc.environment.*"
echo "Libraries are cached in the shared Cabal store - rerun this script"
echo "only when bumping xmonad/xmonad-contrib versions."

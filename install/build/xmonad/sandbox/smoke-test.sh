#!/bin/sh
#
# Build xmonad + xmonad-contrib using cabal
#
# Prerequisites:
#   - GHC, check that version is tested
#   - System C libraries: libX11, libXrandr, libXext, libXinerama, libXScrnSaver
#     Artix/Arch: pacman -S libx11 libxrandr libxext libxinerama libxss
#   - autoconf (for X11 Haskell package)

set -e
XMONAD_VER="0.18.1"
XMONAD_CONTRIB_VER="0.18.2"

BUILD_DIR=~/repos/dotfiles/install/build/xmonad
WORK_DIR=$BUILD_DIR/sandbox/default-build

if command -v ghc >/dev/null 2>&1; then
    echo "Using $(ghc --version)"
else
    echo "Error: no ghc found on PATH"
    exit 1
fi
if command -v cabal >/dev/null 2>&1; then
    echo "and cabal-install version, $(cabal --numeric-version)"
else
    echo "Error: no cabal found on PATH"
    exit 1
fi


cd $WORK_DIR

# Create a simple xmonad file using defaults
echo ""
echo "Create a simple xmonad file using defaults"
cat > ./xmonad-default.hs << EOF
import XMonad

main :: IO ()
main = xmonad def
EOF

# Create a simple cabal file that includes default configuration
echo ""
echo "Create a simple cabal file that includes default configuration"
cat > ./xmonad-def.cabal << EOF
cabal-version:      3.0
name:               xmonad-def
version:            0.1.0

build-type:         Simple
common warnings
    ghc-options: -Wall
executable xmonad-def
    import:           warnings
    main-is:          xmonad-default.hs
    build-depends:    base           >=4.21.2
                    , xmonad         ==${XMONAD_VER}
                    , xmonad-contrib ==${XMONAD_CONTRIB_VER}
    default-language: GHC2021
EOF


echo ""
echo "=== Build and install default  ==="
cabal build && cabal install --overwrite-policy=always

echo ""
echo "success:"
xmonad-def --version


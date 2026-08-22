#!/bin/sh
#
set -e
XMONAD_VER="0.18.1"
XMONAD_CONTRIB_VER="0.18.2"

BUILD_DIR=~/repos/dotfiles/install/build/xmonad
WORK_DIR=$BUILD_DIR/cabal-build
CONFIG_SOURCE=~/repos/dotfiles/config/xmonad/.config/xmonad/xmonad.hs

mkdir -p "$WORK_DIR"
cd $WORK_DIR || exit
cp $CONFIG_SOURCE .

cat > xmonad-rc.cabal << EOF
cabal-version:      3.0
name:               xmonad-rc
version:            0.1.0

build-type:         Simple
common warnings
    ghc-options: -Wall
executable xmonad-rc
    import:           warnings
    main-is:          xmonad.hs
    build-depends:    base           >=4.21.2
                    , xmonad         ==${XMONAD_VER}
                    , xmonad-contrib ==${XMONAD_CONTRIB_VER}
    default-language: GHC2021
EOF

cabal build && cabal install --overwrite-policy=always

# Health check
if command -v xmonad-rc >/dev/null; then
	echo ""
        xmonad-rc --version
else
        echo "Health check: FAIL — binary did not respond to --version"
fi


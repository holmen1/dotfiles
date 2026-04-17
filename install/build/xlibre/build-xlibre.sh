export XLIBRE_SRC="$(pwd)"
export XLIBRE_BUILD="${XLIBRE_SRC}/build"
export XLIBRE_PREFIX="${XLIBRE_SRC}/image"

# Set version variable
VERSION="<version>"

# Check for --get flag
if [[ "$1" == "--get" ]]; then
    wget "https://github.com/X11Libre/xserver/releases/download/${VERSION}/xserver-${VERSION}.tar.gz"
    mkdir -p "$XLIBRE_SRC/xserver"
    tar -xzf xserver-${VERSION}.tar.gz -C "$XLIBRE_SRC/xserver" --strip-components=1
fi


#!/bin/sh

if [ -z "$1" ]; then
  echo "Usage: $0 <config-file>"
  exit 1
fi

CONFIG_FILE="$1"
STOW_DIR="$HOME/repos/dotfiles/config"
TARGET="$HOME"

mkdir -p "$TARGET/.scripts" || exit 1 # TODO: handle adding to .scripts from multiple packages better, maybe with a separate stow package for scripts? For now just make sure this exists before stow tries to link into it.

if [ -f "$CONFIG_FILE" ]; then
  . "$CONFIG_FILE"
else
  echo "Error: Config file '$CONFIG_FILE' not found!"
  exit 1
fi

if [ -z "$packages" ]; then
  echo "Error: No packages defined in config file!"
  exit 1
fi

echo "Using config file: $CONFIG_FILE"
echo ""

case " $packages " in
    *" common "*)
        if [ -L "$TARGET/.xinitrc" ]; then
            old_link=$(readlink "$TARGET/.xinitrc") || exit 1
            case "$old_link" in /*) ;; *) old_link=$TARGET/$old_link ;; esac
            old_dir=$(CDPATH= cd "$(dirname "$old_link")" 2>/dev/null && pwd -P) || old_dir=
            stow_dir=$(CDPATH= cd "$STOW_DIR" && pwd -P) || exit 1
            case "$old_dir/$(basename "$old_link")" in
                "$stow_dir/artixinstall/.xinitrc"|"$stow_dir/bsdinstall/.xinitrc")
                    ln -sf "repos/dotfiles/config/common/.xinitrc" "$TARGET/.xinitrc" || exit 1
                    ;;
            esac
        fi
        ;;
esac

failed=0
for package in $packages; do
    # Dry run to find intended link targets, --adopt needed here to prevent escape
    dry=$(stow -n -v -R --adopt --ignore='.*\.md' -d "$STOW_DIR" -t "$TARGET" "$package" 2>&1)
    # Back up any real files that would conflict
    if ! printf '%s\n' "$dry" | awk '/^LINK:/{print $2}' | while read -r target; do
        full="$HOME/$target"
        if [ -f "$full" ] && [ ! -L "$full" ]; then
            mv "$full" "${full}.bak" || exit 1
            echo "$package: backed up $target"
        fi
    done; then
        echo "Error: could not back up conflicts for '$package'." >&2
        failed=1
        continue
    fi
    # Restow
    if ! stow -R -v --ignore='.*\.md' -d "$STOW_DIR" -t "$TARGET" "$package" 2>&1; then
        failed=1
    fi
    #echo "$package: Stowed"
done

if [ "$failed" -ne 0 ]; then
    echo "Error: one or more packages could not be linked." >&2
    exit 1
fi
echo "Done."


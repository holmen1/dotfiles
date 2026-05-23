#!/bin/sh

draw() {
  kitten icat --stdin no --transfer-mode memory --place "${w}x${h}@${x}x${y}" "$1" </dev/null >/dev/tty
  exit 1
}

file="$1"
w="$2"
h="$3"
x="$4"
y="$5"

# Get the file's MIME type
mimetype=$(file --mime-type -Lb "$file")

case "$mimetype" in
    image/*)
        # Use kitty for terminal image preview
        if command -v kitten >/dev/null 2>&1; then
            draw "$file"
        # Use viu for basic terminal image preview
        elif command -v viu >/dev/null 2>&1; then
            viu -w "$((w-2))" -h "$((h-2))" "$file"
        else
            # Fallback: show image information
            echo "Image: $(basename "$file")"
            identify "$file" 2>/dev/null || file -b "$file"
            echo
            echo "Install 'viu' for terminal image preview"
            echo "Or press '<enter>' to open with feh"
        fi
        ;;
    text/*)
        # Preview text files
        head -n $((h-2)) "$file"
        ;;
    *)
        # For other file types, show file info
        file -b "$file"
        ;;
esac


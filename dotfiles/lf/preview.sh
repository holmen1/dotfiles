#!/bin/bash

file="$1"
w="$2"
h="$3"
x="$4"
y="$5"

# Get the file's MIME type
mimetype=$(file --mime-type -Lb "$file")

case "$mimetype" in
    image/*)
        # Use viu for basic terminal image preview
        if command -v viu >/dev/null 2>&1; then
            viu -w "$((w-2))" -h "$((h-2))" "$file"
        else
            # Fallback: show image information
            echo "Image: $(basename "$file")"
            identify "$file" 2>/dev/null || file -b "$file"
            echo
            echo "Install 'viu' for terminal image preview"
            echo "Or press 'f' to open with feh"
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
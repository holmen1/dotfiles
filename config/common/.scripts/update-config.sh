#!/bin/sh

# Replace a marked multiline shell variable with the contents of a file.
update_config_section() (
    config=$1
    variable=$2
    input=$3

    case "$variable" in
        ''|[!a-zA-Z_]*|*[!a-zA-Z0-9_]*)
            echo "Error: invalid configuration variable '$variable'" >&2
            return 1
            ;;
    esac

    temp=$(mktemp "${config}.XXXXXX") || return 1
    trap 'rm -f "$temp"' 0
    trap 'exit 1' HUP INT TERM
    cp -p "$config" "$temp" || return 1

    if ! awk -v variable="$variable" -v input="$input" '
        BEGIN {
            begin = "# BEGIN " variable
            end = "# END " variable
            quote = sprintf("%c", 39)
        }
        $0 == begin {
            if (inside || found++) exit 1
            inside = 1
            print
            print variable "=" quote
            while ((result = getline line < input) > 0) {
                count = split(line, parts, quote)
                printf "%s", parts[1]
                for (i = 2; i <= count; i++) printf "%s\\%s%s%s", quote, quote, quote, parts[i]
                print ""
            }
            if (result < 0) exit 1
            close(input)
            print quote
            next
        }
        $0 == end {
            if (!inside) exit 1
            inside = 0
            print
            next
        }
        !inside { print }
        END { if (!found || inside) exit 1 }
    ' "$config" > "$temp"; then
        echo "Error: could not update '$variable' in '$config'" >&2
        return 1
    fi
    mv -f "$temp" "$config"
)

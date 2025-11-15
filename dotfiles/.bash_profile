#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [[ -z $DISPLAY && $(tty) = /dev/tty1 ]]; then
    clear
    echo "" # Add a newline for spacing
    fastfetch
    echo "" # Add a newline for spacing

    for i in {6..1}; do
        # \r returns to the start of the line
        printf "Starting X in %s Press Ctrl+C to abort \r" "$i"
        sleep 1
    done

    echo "Starting X now...           " # The spaces clear the previous line
    exec startx
fi

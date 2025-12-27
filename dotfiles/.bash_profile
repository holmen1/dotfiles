#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# Auto-startx on first tty (tty1 on Linux, ttyv0 on FreeBSD)
if [[ -z $DISPLAY ]] && { [[ $(tty) = /dev/tty1 ]] || [[ $(tty) = /dev/ttyv0 ]]; }; then
    clear
    echo "" # Add a newline for spacing
    fastfetch
    echo "" # Add a newline for spacing

    for i in {6..1}; do
        # Force output flush and overwrite the same line
        printf "\rStarting X in %s... Press Ctrl+C to abort" "$i"
        sleep 1
    done

    # Clear the countdown line and show final message
    printf "\rStarting X now...                          \n"
    sleep 0.5
    
    # Clear terminal completely before starting X to prevent artifacts
    clear
    exec startx
fi

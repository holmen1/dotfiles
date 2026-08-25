#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

echo -n ""

bsd_info() {
        [ "$(cat "$HOME/.cache/bsd_attempt")" = "1" ] && echo "BSD need 2 runs (Xlibre issue)"
}

# Auto-startx on first tty (tty1 on Linux, ttyv0 on FreeBSD)
if [[ -z $DISPLAY ]] && { [[ $(tty) = /dev/tty1 ]] || [[ $(tty) = /dev/ttyv0 ]]; }; then
    clear
    echo ""
    fastfetch --logo "none"
    echo ""

    # Startup info BSD only
    [ $(tty) = /dev/ttyv0 ] && echo "1" > $HOME/.cache/bsd_attempt && bsd_info

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
    # XLibre on FreeBSD needs a kept tty so libseat stays active on first start.
    exec startx -- -keeptty
fi

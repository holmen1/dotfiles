#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# Change directory open vscode
cdc() {
	cd "$1" && code .
}

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	echo "Starting X in 5 seconds. Press Ctrl+C to abort."
	sleep 5
	exec startx
fi

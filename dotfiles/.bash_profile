#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	echo "Starting X in 5 seconds. Press Ctrl+C to abort."
	sleep 5
	exec startx
fi

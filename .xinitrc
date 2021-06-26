#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/local/etc/X11/xinit/.Xresources
sysmodmap=/usr/local/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -d /usr/local/etc/X11/xinit/xinitrc.d ] ; then
	for f in /usr/local/etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

# Thanks, https://github.com/johanwiden/exwm-setup#support-for-encryption-gpg
# gpg has to be started here (or in .xinitrc.exwm), if we want to have encryption in exwm
gpg-connect-agent /bye
SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export SSH_AUTH_SOCK

xhost +SI:localuser:$USER
xhost +SI:localuser:root
xsetroot -cursor_name left_ptr
xset r rate 200 60
redshift &
exec emacs --maximized

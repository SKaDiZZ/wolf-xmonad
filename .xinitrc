#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Set cursor
    xsetroot -cursor_name left_ptr

# Set keyboard
    setxkbmap hr 

# Nitrogen
    nitrogen --restore &

# Clipboard manager
    clipit &

# VolWheel
    volwheel &

# Start compositing
	  compton -cC -fF -i 0.8 &

# Start thunar daemon
    thunar --daemon &

    sleep 2s &

exec xmonad

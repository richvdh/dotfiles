# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

if [ -x "/usr/bin/keychain" ]; then
    eval keychain --eval --inherit any
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

export DEBFULLNAME='Richard van der Hoff'
export DEBEMAIL='launchpad@rvanderhoff.org.uk'

export INPUTRC=$HOME/dotfiles/inputrc
export LYNX_CFG=$HOME/dotfiles/lynx.cfg

# try to pick an EDITOR
if [ -x "$HOME/bin/edit" ]; then
    EDITOR="$HOME/bin/edit"
elif [ -x "/usr/bin/emacsclient" ]; then
    EDITOR=/usr/bin/emacsclient
elif [ -x "/usr/bin/emacs" ]; then
    EDITOR="/usr/bin/emacs"
elif [ -x "/usr/bin/jed" ]; then
    EDITOR="/usr/bin/jed"
fi


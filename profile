# -*- mode: shell-script -*-
#
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
    eval `keychain --eval --inherit any`
fi

# set PATH so it includes user's private bin if it exists
export PATH="$HOME/bin:$HOME/.local/bin:$HOME/gocode/bin:$PATH"

export DEBFULLNAME='Richard van der Hoff'
export DEBEMAIL='launchpad@rvanderhoff.org.uk'

export INPUTRC=$HOME/dotfiles/inputrc
export LYNX_CFG=$HOME/dotfiles/lynx.cfg

# try to pick an EDITOR
if [ -x "$HOME/bin/edit" ]; then
    EDITOR="$HOME/bin/edit"
elif [ -x "/usr/bin/emacsclient" ]; then
    EDITOR=$HOME/dotfiles/emacsclient-wrapper
elif [ -x "/usr/bin/emacs" ]; then
    EDITOR="/usr/bin/emacs"
elif [ -x "/usr/bin/jed" ]; then
    EDITOR="/usr/bin/jed"
fi

export EDITOR

export HOSTNAME=`hostname`
if [ -f $HOME/dotfiles/profile.$HOSTNAME ]; then
   . $HOME/dotfiles/profile.$HOSTNAME
fi

export IPOD_MOUNTPOINT=/media/rav/RICHARD\'S\ I

export GOPATH=$HOME/gocode

# let cpan install to ~
export PERL5LIB=$HOME/lib/perl5
export PERL_MB_OPT=--install_base=$HOME
export PERL_MM_OPT=INSTALL_BASE=$HOME

# colours are nice
export LESS=-R

# maths is nice
export BC_ENV_ARGS="-l $HOME/.bcrc"

# load emscripten stuff
if [ -d "$HOME/emsdk_portable" ]; then
   # emsdk_portable/emsdk_env.sh is a thing, which is auto-maintained by 'emsdk
   # activate' and friends. However it adds its own clang to the path, which is
   # unnecessary, and conflicts with normal clang use.
   export PATH="/home/rav/emsdk_portable:/home/rav/emsdk_portable/emscripten/master:$PATH"
   export EM_CONFIG="/home/rav/.emscripten"
   export EMSCRIPTEN="/home/rav/emsdk_portable/emscripten/master"
fi

# add cargo to $PATH
if [ -f "$HOME/.cargo/env" ]; then
    . "$HOME/.cargo/env"
fi

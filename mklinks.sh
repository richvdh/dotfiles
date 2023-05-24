#!/bin/sh

mydir=$(dirname "$(readlink -f "$0")")

set -e

link()
{
    target="$mydir/$1"
    link=$2

    mkdir -vp "$(dirname "$link")"

    if [ ! -e "$link" ]; then 
        ln -vs "$target" "$link"
        return
    fi

    currenttarget=`readlink -f "$link"`
    if [ "$currenttarget" = "$target" ]; then
        echo "$link -> $target already in place"
        return
    fi

    echo "$link exists and is not a link to $target" >&2
    exit 1
}

inodeno()
{
    ls -i "$1" | awk '{ print $1 }'
}

hardlink()
{
    target="$mydir/$1"
    link=$2

    if [ ! -e "$link" ]; then 
        ln "$target" "$link"
        return
    fi
    
    if [ `inodeno $link` -eq `inodeno $link` ]; then
        echo "$link => $target already in place"
        return
    fi

    echo "$link exists and is not a hardlink of $target" >&2
    exit 1
}

link "emacs" ~/.emacs
link "bash_aliases" ~/.bash_aliases
link "bashrc" ~/.bashrc
link "profile" ~/.profile

case `uname -o` in
    Cygwin)
        # can't use a symlink here
        hardlink "gitconfig.cygwin" ~/.gitconfig
        ;;
    *)
        link "gitconfig" ~/.gitconfig
        ;;
esac

link "inputrc" ~/.inputrc
link "lynxrc" ~/.lynxrc
link "ssh_config" ~/.ssh/config
link "svn_config" ~/.subversion/config
link "hgrc/hgrc" ~/.hgrc
link "bcrc" ~/.bcrc
link "gdbinit" ~/.gdbinit
link "upstart" ~/.config/upstart
link "devscripts" ~/.devscripts
link "dput.cf" ~/.dput.cf
link "xsessionrc" ~/.xsessionrc
link "direnvrc" ~/.direnvrc

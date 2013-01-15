#!/bin/sh

mydir=$(dirname "$(readlink -f "$0")")

link()
{
    target="$mydir/$1"
    link=$2

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

link "bashrc" ~/.bashrc
link "profile" ~/.profile
link "gitconfig" ~/.gitconfig
link "emacs" ~/.emacs

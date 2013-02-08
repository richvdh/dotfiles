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

copy()
{
    target="$mydir/$1"
    link=$2

    if [ ! -e "$link" ]; then 
        cp -v "$target" "$link"
        return
    fi
    
    if cmp -s $link $target; then
        echo "$link == $target already in place"
        return
    fi

    echo "$link exists and is not a copy of $target" >&2
    exit 1
}

link "bashrc" ~/.bashrc
link "profile" ~/.profile

case `uname -o` in
    Cygwin)
        # can't use a copy here
        copy "gitconfig.cygwin" ~/.gitconfig
        ;;
    *)
        link "gitconfig" ~/.gitconfig
        ;;
esac

# link "emacs" ~/.emacs

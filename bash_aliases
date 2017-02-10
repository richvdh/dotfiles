function pr {
    if [ "$1" == "-r" ]; then
        rm -f `git rev-parse --git-dir`/PULLREQ_EDITMSG
        shift
    fi
    hub pull-request -o "$@"
}

# enable colours for jq even when piping to less
alias jq='jq -C'


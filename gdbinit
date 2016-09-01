add-auto-load-safe-path /usr/local/go-*/src/runtime/runtime-gdb.py
add-auto-load-safe-path /home/rav/linux/linux/scripts/gdb/vmlinux-gdb.py
set history save
set print pretty on

# dunno why this isn't auto-loaded. possibly because it's in a subdir?
# it doesn't actually work when it's auto-loaded. sigh.
# source /usr/share/gdb/auto-load/usr/bin/python2.7-dbg-gdb.py

# --L.


# Read the kernel command line variable 'hostname'; this crude hack is
# needed because filesystems are mounted read-only at this stage
export `tr ' ' '\n' < /proc/cmdline 2> /dev/null | grep hostname=` &> /dev/null

# Show a first message in the terminal window title bar...
echo -en '\033]0;'Booting up the virtual machine \"$hostname\"...'\a'

#!/bin/bash
# Usage: ./visualise.sh ./your_program

set -e

PROGRAM="$1"
OUTFILE="gprof.txt"
DOTFILE="profile.dot"
IMGFILE="profile.svg"

if [ -z "$PROGRAM" ]; then
    echo "Usage: $0 ./your_program"
    exit 1
fi

# Generate flat gprof output
gprof "$PROGRAM" "$(dirname "$1")/gmon.out" > "$OUTFILE"

# Convert to dot format
gprof2dot -f prof "$OUTFILE" -o "$DOTFILE"

# Generate SVG image
dot -Tsvg "$DOTFILE" -o "$IMGFILE"

# Done
xdg-open "$IMGFILE" &> /dev/null || echo "Open $IMGFILE manually."

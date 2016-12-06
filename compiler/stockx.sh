#!/bin/bash

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 [stockx_file] [flag] [output_file]" 1>&2
    exit 1
fi

MYDIR="$(dirname "$(which "$0")")"
STOCKX_FILE="$MYDIR/stockx"

cat $1 | $STOCKX_FILE $2 > $3

exit 0

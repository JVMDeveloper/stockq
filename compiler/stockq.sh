#!/bin/bash

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 [stockq_file] [flag] [output_file]" 1>&2
    exit 1
fi

MYDIR="$(dirname "$(which "$0")")"
STOCKQ_FILE="$MYDIR/stockq"

cat $1 | $STOCKQ_FILE $2 > $3

exit 0

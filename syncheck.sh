#!/bin/bash

set -e

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)
PROG=syncheck.sh

trap bug_found ERR

function usage() {
    echo "$PROG"
}

function bug_found() {
    echo >&2 "ERROR: You found a bug in the $PROG script, please report"
    exit 99
}

cd "$HERE"

RET=0

#file test args msg
function check() {
    if grep -q $3 "$2" -- "$1"; then
        echo "SYNTAX ERROR: $4 found in '$1'"
        grep --color=always $3 "$2" -- "$1" | while read line; do
            echo " --> $line"
        done
        echo
        RET=1
    fi
    return 0
}

while read file; do
    check "$file" '^.{81,}$' -EHn "line too long"
    check "$file" ' $' -EHn "trailing whitespace"
    check "$file" '	' -Hn "tab found"
done < <(find . \(     -name '*.h' \
                   -or -name '*.cpp' \
                   -or -name '*.c' \
                   -or -name '*.hs' \
                   -or -name '*.chs' \
                   -or -name '*.tex' \
                   -or -name '*.sh' \
                \) \
                -and -not \( -path './dist/*' \
                             -or -path './syncheck.sh' \
                          \) \
                -type f \
        )

if [ $RET -eq 0 ]; then
    echo "CONGRATULATIONS, looks fine"
fi

exit $RET

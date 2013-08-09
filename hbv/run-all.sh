#!/bin/sh
for var in "$@"
do
    echo "$var"
    time (./dist/build/hbv/hbv -b) 2>&1 < $var
    echo ""
done

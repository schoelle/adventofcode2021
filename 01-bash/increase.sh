#!/bin/bash

oldnum=1000000
for number in $(cat $*); do
    if [ $number -gt $oldnum ]; then
	echo "."
    fi
    oldnum=$number
done | wc -l

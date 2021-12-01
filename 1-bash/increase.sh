#!/bin/bash

FILE=input.txt
OLDNUM=1000000
COUNT=0
for number in $(cat $FILE); do
    if [ $number -gt $OLDNUM ]; then
	echo "Increase"
    fi
    OLDNUM=$number
done | wc -l

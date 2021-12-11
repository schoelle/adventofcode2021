#!/bin/bash

FILE=input.txt

numA=100000
numB=100000
numC=100000
for number in $(cat $*); do
    newA=$numB
    newB=$numC
    newC=$number

    sumOld=$(( $numA + $numB + $numC ))
    sumNew=$(( $newA + $newB + $newC ))
    
    if [ $sumNew -gt $sumOld ]; then
	echo "."
    fi
    numA=$newA
    numB=$newB
    numC=$newC
done | wc -l

#!/bin/sh
awk -f tobinary.awk < $1 | bfi -b lanternfish.bf | wc -c

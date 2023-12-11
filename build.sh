#!/bin/env bash

# Compiler and flags
fc=gfortran
flags="-Wall -Wextra -Wno-unused-variable -Wno-maybe-uninitialized -fcheck=bounds"
# flags="${flags} -fno-range-check"
flags="${flags} -g -fbacktrace"

# echo "$flags"

# Build the binary
#$fc --version

$fc -o main ../utils.f* main.f* $flags

if [ $# -gt 0 ]; then
    case "$1" in
        r*)
            ./main | tee >(awk '{if ($1 ~ /^Part/) {print $3; exit 0;}}' \
                | xclip -selection clipboard)
            ;;
    esac
fi

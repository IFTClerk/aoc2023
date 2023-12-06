#!/bin/env bash

# Compiler and flags
fc=gfortran
flags="-Wall -Wextra -Wno-unused-variable -fcheck=bounds"
# flags="${flags} -fno-range-check"

# echo "$flags"

# Build the binary
#$fc --version

$fc -o main ../utils.f* main.f* $flags

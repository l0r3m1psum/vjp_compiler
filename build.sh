#!/bin/sh

set -e

cc -g -std=c17 matrixcalculus.c -o matrixcalculus
cc -g -std=c17 main.c -o main
main

#!/bin/sh

set -e

clang++ -shared -undefined dynamic_lookup -std=c++17 \
  -I ~/Developer/llvm-project/lldb/include/ \
  /Library/Developer/CommandLineTools/Library/PrivateFrameworks/LLDB.framework/Versions/A/LLDB \
  -o types.dylib types.cpp
cc -g -std=c17 matrixcalculus.c -o matrixcalculus &
cc -g -DTEST -std=c17 main.c -o main
main

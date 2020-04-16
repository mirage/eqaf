#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu" ]]; then
  nasm -felf64 asm_sleep.S
elif [[ "$OSTYPE" == "darwin"* ]]; then
  nasm -fmacho64 asm_sleep.S
elif [[ "$OSTYPE" == "msys" ]]; then
  nasm -fwin64 asm_sleep.S
elif [[ "$OSTYPE" == "cygwin" ]]; then
  nasm -fwin64 asm_sleep.S
else
  echo "Impossible to compile asm_sleep.S (unknown OS: $OSTYPE)"
fi

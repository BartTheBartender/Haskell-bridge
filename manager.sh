#!/bin/bash

if [ "$1" == "clear" ]; then
  cd ~/.local/state/nvim/swap
  rm -rf *PF2%projekt*
elif [ "$1" == "compile" ]; then
  ghc -hidir hi -odir o main.hs
elif [ "$1" == "howmany" ]; then
  find . -type d -name .git -prune -o -type f -print0 | xargs -0 wc -l
else
  echo "Usage: $0 {clear|compile|git <$2>}"
  exit 1
fi


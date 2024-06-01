#!/bin/bash

if [ "$1" == "clear" ]; then
  cd ~/.local/state/nvim/swap
  rm -rf *PF2%projekt*
elif [ "$1" == "compile" ]; then
  ghc -hidir hi -odir o main.hs
elif [ "$1" == "git" ]; then
  git apdejtujkurwa
  git takkurwa $2
  git nakurwiaj
else
  echo "Usage: $0 {clear|compile|git <$2>}"
  exit 1
fi


#!/bin/sh

cd "$(dirname "$0")" || exit

find ../* -name '*.hs' -exec \
  brittany \
    --write-mode inplace \
    {} \
  \;

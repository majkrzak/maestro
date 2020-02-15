#!/bin/sh

cd "$(dirname "$0")"

find ../* -name '*.hs' -exec \
  brittany \
    --write-mode inplace \
    {} \
  \;

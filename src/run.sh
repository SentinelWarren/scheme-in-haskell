#!/bin/bash -e
ghc main.hs
rlwrap ./main "$1"

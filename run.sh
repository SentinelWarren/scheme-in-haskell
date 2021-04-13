#!/bin/bash -e

if [ "$1" == "--test" ];
then
    ghc src/Main.hs
    runhaskell -i./src test/Spec.hs
else
    ghc src/Main.hs
    rlwrap src/Main
fi

# rlwrap ./main "$1"

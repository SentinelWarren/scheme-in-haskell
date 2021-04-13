#!/bin/bash -e

if [ "$1" == "--test" ];
then
    runhaskell -i./src -i./app test/Spec.hs
else
    ghc -i./src app/Main.hs
    rlwrap app/Main
    rm src/*.o src/*.hi app/*.o app/*.hi
fi

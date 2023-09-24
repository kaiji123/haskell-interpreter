#!/bin/sh
runhaskell --ghc-arg="-package mtl" RunOptimize.hs $1

#!/bin/bash
set -e
set -u

ghc  --make -prof -auto-all -caf-all -fforce-recomp -O2 Main.hs

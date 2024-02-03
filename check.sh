#!/usr/bin/env sh
set -e
hlint .
stack clean
stack test --ghc-options="-Werror $2" --ta "$1 $(cat solved-tasks.txt)"

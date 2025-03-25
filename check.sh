#!/usr/bin/env sh
set -e
hlint .
stack clean
stack build --test --no-run-tests # magic to increase a probability of success
stack test --ghc-options="-Werror $2" --ta "$1 $(cat solved-tasks.txt)"

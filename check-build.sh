#!/usr/bin/env sh
set -e
hlint .
stack clean
stack build --test --no-run-tests --ghc-options='-Werror'

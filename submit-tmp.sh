#!/usr/bin/env sh
set -e
if git log -n 1 | grep 'tmp';
then
  git commit src/. -m '[no ci] tmp' --amend
else
  git commit src/. -m '[no ci] tmp'
fi
git push --force origin main:main
echo "Do not forget to push final version to CI!"

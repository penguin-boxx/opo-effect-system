#!/usr/bin/env sh
set -e
sh check.sh "$1"
if git log -n 1 | grep -E 'tmp|hard';
then
  git commit src/. -m "hard: $1 $(cat solved-tasks.txt)" --amend
else
  git commit src/. -m "hard: $1 $(cat solved-tasks.txt)" --allow-empty
fi
git push --force origin main:main
echo "Good job! Send email and wait for results!"

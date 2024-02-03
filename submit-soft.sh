#!/usr/bin/env sh
set -e
sh check.sh "$1"
if git log -n 1 | grep 'soft';
then
  echo "You are submitting to soft deadline once again!"
  git commit src/. -m "soft: $1 $(cat solved-tasks.txt)" --amend
else
  git commit src/. -m "soft: $1 $(cat solved-tasks.txt)" --allow-empty
fi
git push --force origin main:main
echo "Good job! Send email and wait for results!"

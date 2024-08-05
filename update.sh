#!/usr/bin/env sh
# For course staff only. Update homework.
# Example: sh update.sh "Commit message"
set -e

git push origin main:main # To have main as a default branch on github (first push)

git checkout solutions
git pull upstream main
sh check.sh "" "-Wunused-imports -Wredundant-constraints"
if [ "$1" = "" ]
then
  git commit -am "[no ci] Update" --allow-empty
else
  git commit -am "[no ci] $1" --allow-empty
fi
git push origin solutions:solutions

git checkout main
git merge solutions -m "[no ci] Merged with solutions"
sh check-build.sh

git push origin main:main
git push public main:main
git checkout solutions

#!/usr/bin/env bash
# For course staff only. Updates homework in template repo.
# Expects:
#  * remote origin - homework template
#  * remote template - internal template
#  * remote public - homework template in the classroom organization
# Example: ./update.sh "Commit message"
# Example: ./update.sh --all
set -e

if [ "$1" = "--all" ]
then
  SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
  echo "Script found itself in $SCRIPT_DIR"
  for dir in "$(dirname "$SCRIPT_DIR")"/*/
  do
  	dir=${dir%*/} # remove the trailing "/"
  	echo ">>> Updating $dir..."
  	cd "$dir"
  	bash update.sh
  	echo ">>> Done for $dir!"
  done
  exit 0
fi

git push origin main:main # To have main as a default branch on github (first push)

git config pull.rebase false

git checkout solutions
sh check.sh "" "-Wunused-imports -Wredundant-constraints"
if [ "$1" = "" ]
then
  git commit -am "[no ci] Update" --allow-empty
else
  git commit -am "[no ci] $1" --allow-empty
fi
git pull template main --no-edit
git push origin solutions:solutions

git checkout main
git merge solutions -m "[no ci] Merged with solutions"
sh check-build.sh
git push origin main:main
git push public main:main

git checkout solutions

set -e # https://stackoverflow.com/questions/1378274/in-a-bash-script-how-can-i-exit-the-entire-script-if-a-certain-condition-occurs
echo "Running test $1"
stack test --system-ghc --ta "$1"

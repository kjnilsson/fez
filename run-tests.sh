set -e
./fezc ./test/basics.fs
erlc -o ./ebin/ ./test/*.core
make tests

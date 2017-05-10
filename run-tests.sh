set -e
./fezc ./test/basics.fs
erlc -o ./ebin/ ./test/basics.core
make tests

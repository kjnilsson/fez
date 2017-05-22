set -e
# ./fezc ./test/Aether.fs ./test/aether_test.fs ./test/basics.fs
./fezc ./test/basics.fs
erlc -o ./test ./test/*.core
make tests

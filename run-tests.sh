set -e
./fezc ./test/basics.fs
erlc -o ./ebin ./src/*.erl
escript test/basics.escript

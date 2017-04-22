set -e
./fezc ./test/basics.fs
erlc ./test/*.erl
escript test/basics.escript

set -e
./fezc ./test/basics.fs
erlc -o ./test ./test/basics*.core
erlc -o ./test ./test/*.erl
erl -noshell -pa ebin test -eval "eunit:test(basics_tests, [verbose])" -s init stop

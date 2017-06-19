set -e
./fezc ./test/basics.fs
erlc -o ./ebin -DTEST=1 ./src/*.erl
erlc -o ./test -DTEST=1 ./test/*.erl
m=$(grep -l 'include/eunit.hrl' **/*.erl | xargs basename -s ".erl" | awk '{print}' ORS="','" | rev | cut -c 3- | rev)
erl -noshell -pa ./ebin ./test -eval "case eunit:test(['$m], [verbose]) of ok -> ok; error -> halt(1) end" -s init stop

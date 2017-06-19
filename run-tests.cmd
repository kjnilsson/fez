dotnet run -p src/fez/fez.fsproj test\basics.fs
FOR %%f in (test\*.erl) DO erlc +clint -o test "%%f"
erl -noshell -pa .\ebin .\test -eval "eunit:test(basics_tests, [verbose])" -s init stop

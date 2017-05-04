set -e
mkdir -p ebin
dotnet restore src/fez/fez.fsproj
dotnet restore ./test/test.fsproj
dotnet build src/fez/fez.fsproj
erlc +clint -o ./ebin ./src/*.erl

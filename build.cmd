mkdir ebin
dotnet restore src/fez/fez.fsproj
dotnet restore ./test/test.fsproj
dotnet build src/fez/fez.fsproj
FOR %%f in (src\*.erl) DO erlc +clint -o ebin "%%f"
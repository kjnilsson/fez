echo "compiling: %*"
dotnet run -p src/fez/fez.fsproj "%*"
erlc -v -o test "test/basics.core" rem get file names

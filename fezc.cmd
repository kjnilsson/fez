@echo off

echo compiling: %*
for /f %%l in ('dotnet run -p src/fez/fez.fsproj "%*"') do (
    erlc -v -o test %%l.core
)
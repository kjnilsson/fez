@echo off

for /f %%l in ('dotnet run -p src/fez/fez.fsproj %*') do (
    For %%A in (%%l) do erlc -v -o %%~dpA %%l
)
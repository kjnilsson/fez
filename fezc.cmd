@echo off


setlocal enableDelayedExpansion

for /f %%l in ('dotnet run -p src/fez/fez.fsproj %*') do (
    if ERRORLEVEL 1 echo %%l && goto :failure
    For %%A in (%%l) do erlc -v -o %%~dpA %%l
    if ERRORLEVEL 1 echo Error: erlc failed && goto :failure
)

goto :success
REM ------ exit -------------------------------------
:failure
echo bad
endlocal
exit /b 1

:success
echo good
endlocal
exit /b 0

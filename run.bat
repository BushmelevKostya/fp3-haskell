@echo off

stack build

if %errorlevel% equ 0 (
    stack exec fp3-haskell-exe
) else (
    echo build failed
) 
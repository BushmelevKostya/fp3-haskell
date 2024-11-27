@echo off

set STEP=0.6 
set ALGOTYPE=2 # 1 for Linear, 2 for Quadratic, 3 for Lagrange, 4 for All

stack build

if %errorlevel% equ 0 (
    stack exec fp3-haskell-exe %STEP% %ALGOTYPE%
) else (
    echo build failed
) 
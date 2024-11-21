@echo off

rem https://devblogs.microsoft.com/cppblog/addresssanitizer-asan-for-windows-with-msvc/

rem cl /nologo /Zi /fsanitize=address /std:c17 main.c
cl /nologo /Zi /std:c17 main.c /JMC /link /natvis:types.natvis || exit /b

main
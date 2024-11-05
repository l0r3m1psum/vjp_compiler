@echo off

rem https://devblogs.microsoft.com/cppblog/addresssanitizer-asan-for-windows-with-msvc/

cl /nologo /Zi /fsanitize=address /std:c17 main.c
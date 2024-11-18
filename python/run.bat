@echo off
setlocal
chcp 65001
set TORCH_LOGS=+all
rem To generate the torch_compile_debug
set TORCH_COMPILE_DEBUG=1
set TORCH_LOGS_OUT=torch_logs_out.txt
rem For python310.lib
set "LIB=%LIB%;%LOCALAPPDATA%\Programs\Python\Python310\libs"
py compile.py
endlocal
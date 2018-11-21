@echo off
for /f "tokens=*" %%a in (ascii.dat) do (
call %%a
)
pause
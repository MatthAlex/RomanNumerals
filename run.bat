del *.bak
del *.obj
del *.exe
:: /warn:all

call ifort /nologo /check:all /warn:all /traceback -c modules.f90
call ifort /nologo /check:all /warn:all /traceback RomanNum.f90 modules.obj

:: call ifort /Qipo /nologo /traceback -c modules.f90
:: call ifort /nologo /fast /traceback RomanNum.f90 modules.obj
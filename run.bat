:: rem ifort /F1280000000 /fast /traceback /check:all /warn:all CA_MPI.f90

::call mpif90 /Qipo /nologo -c MPIBoundary2.f90 MPIBoundary1.f90 MPIBoundary1I.f90 SolveFlow.f90 SolvePsi.f90
::call mpif90 /F2560000000 /traceback /fast /nologo CA_MPI.f90 /I"C:\Program Files\Tecplot\Tecplot 360 EX 2016 R1\include" "C:\Program Files\Tecplot\Tecplot 360 EX 2016 R1\lib\tecio.lib" ::MPIBoundary1.obj MPIBoundary1I.obj SolveFlow.obj SolvePsi.obj


:: del *.dat
del *.bak
del *.obj

::mkdir C:\MPI_core3\
::copy machines C:\MPI_core3\
::del C:\MPI_core3\CA_MPI.exe
::copy CA_MPI.exe C:\MPI_core3\


:: rem mpiexec -machinefile machines -genv I_MPI_PIN_PROCS 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30 -n 4  c:\MPIHTRun\MPIHT.exe 
::mpiexec -machinefile machines -genv I_MPI_PIN_PROCS 0,2,1,3 -n 1  C:\MPI_core3\CA_MPI.exe 


call ifort /traceback RomanNum.f90
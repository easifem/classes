rm *.out
ifort -c -fpp main.f90 -I$include
ifort main.o $lib/*.o -llapack
rm *.mod *.o
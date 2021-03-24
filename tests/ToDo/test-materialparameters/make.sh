rm *.mod *.o *.out
file=$1
ifort $file -I$include $lib/*.o

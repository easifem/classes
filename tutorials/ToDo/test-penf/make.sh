rm *.o *.mod *.smod *.out
file=$1
ifort $file -I$include $lib/*.o

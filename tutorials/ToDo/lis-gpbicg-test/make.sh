file=$1
rm *.out
ifort $file -I$include $lib/*.o -llapack
rm *.mod *.o

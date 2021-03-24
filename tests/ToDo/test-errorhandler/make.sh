rm *.o *.mod *.smod *.out
file=$1
ifort -fpp $file -I$include $lib/*.o

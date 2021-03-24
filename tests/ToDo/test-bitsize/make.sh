rm *.o *.mod *.smod *.out
file=$1
if [ -x ifort ]; then
FC="ifort -fpp"
else
FC="gfortran -cpp"
fi
$FC $file -I$include $lib/*.o

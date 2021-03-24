file=$1
if [ -x ifort ]; then
ifort -fpp $file
else
gfortran -cpp $file
fi
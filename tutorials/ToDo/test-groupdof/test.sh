cd $include

fc=ifort
flag=-DMULTI_THREADED
fopt="-fpp -c -qopenmp"
#fopt="-fpp -c"

echo "GroupDOF_Class.f90..."

$fc $flag $fopt $soafem/Classes/GroupDOF/GroupDOF_Class.f90
echo "moving files..."
mv -f  ./*.o $soafem/lib

cd -

filename=test
fc=ifort
fopt="-qopenmp -c"

$fc $fopt $filename.f90 \
    -I$include

fopt="-qopenmp"
$fc $fopt $filename.o \
    $lib/*.o \
    -llapack

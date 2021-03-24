#!/bin/bash
FC="gfortran"
CC="gcc"
AR="ar"
WRC=
PRJTK=
RM="rm -f"

IDIR="-I${HOME}/include -I${HOME}/include/extpkgs" 

# LDIR="-L/home/easifem/lib/extpkgs -L/home/easifem/lib" 
LDIR= 
# warning - directory error: ../../../RR

file=$1

OPTFLAGS="-g"

SPECIALFLAGS=${IDIR}

RCFLAGS="-O coff"

PRJ_FFLAGS="-cpp -std=f2008"

PRJ_CFLAGS=

PRJ_LFLAGS="${HOME}/lib/extpkgs/*.o ${HOME}/lib/*.o -lblas -llapack"

FFLAGS="${SPECIALFLAGS} ${OPTFLAGS} ${PRJ_FFLAGS}"

CFLAGS="${SPECIALFLAGS} ${OPTFLAGS} ${PRJ_CFLAGS}"

echo "Cleaning *.o *.out"
rm *.o *.out

echo "Compiling ${file}.f90"
${FC} -c -o ${file}.o ${FFLAGS} ${file}.f90

echo "building main.out"
${FC} -o main.out ${file}.o ${LDIR} ${PRJ_LFLAGS}

echo "executing main.out"
./main.out

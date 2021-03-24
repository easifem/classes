filename=$1
ifort $filename -I$include $lib/*.o -llapack

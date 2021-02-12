rm *.mod *.o *.out
file=$1
ifort $file -I$include $lib/GlobalData.o $lib/IO.o $lib/String_Class.o $lib/*KeyValue_Class*

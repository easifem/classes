program main
  use globaldata
  use bitsize_module
  implicit none

  write( *, * ) "real32", BitSize( 1.0_real32 )
  write( *, * ) "real64", BitSize( 1.0_real64 )
  write(  *, * ) "int8", BitSize( 1_int8 )
  write( *, * ) "int16", BitSize( 1_int16 )
  write( *, * ) "int32", BitSize( 1_int32 )
  write( *, * ) "int64", BitSize( 1_int64 )
  write( *, * ) "char", BitSize( "hello" )

  write( *, * ) "real32", ByteSize( 1.0_real32 )
  write( *, * ) "real64", ByteSize( 1.0_real64 )
  write(  *, * ) "int8", ByteSize( 1_int8 )
  write( *, * ) "int16", ByteSize( 1_int16 )
  write( *, * ) "int32", ByteSize( 1_int32 )
  write( *, * ) "int64", ByteSize( 1_int64 )
  write( *, * ) "char", ByteSize( "hello" )
end program main

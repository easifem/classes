! test key value constructor
program main
  use globaldata
  use keyvalue_class
  use string_class

  type( keyvalue_ ) :: obj
  real( dfp ) :: mat2( 2, 2 )
  type( string_ ) :: str
  character( len = 100 ) :: chr

  mat2 = 1.0_dfp

  call obj % initiate( "hello", 1 )
  call obj % display( )

  call obj % initiate( "hello", [1.0_dfp] )
  call obj % display( )

  call obj % initiate( "real rank 0", 1.0_dfp )
  call obj % display( )

  obj = keyvalue( "real rank 0 ", 1.0_dfp )
  call obj % display( )

  obj = keyvalue( "real rank 1 ", [1.0_dfp] )
  call obj % display( )

  obj = keyvalue( "real rank 2 ", mat2 )
  call obj % display( )

  str = obj
  call str % display( )

  chr = obj
  write( *, * ) "chr = obj :: ", trim( chr )
end program main

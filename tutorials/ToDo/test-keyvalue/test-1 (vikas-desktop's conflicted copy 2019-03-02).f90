! test key value constructor
program main
  use globaldata
  use keyvalue_class
  use string_class

  type( keyvalue_ ) :: obj
  integer( i4b ) :: initiate1
  call obj % initiate( "hello", 1 )
  call obj % display( )

  call obj % initiate( "hello", [1.0_dfp] )
  call obj % display( )

  call obj % initiate( "real rank 0", 1.0_dfp )
  call obj % display( )

  !call initiate1( obj, "hello", 1.0_dfp )
end program main

program main
  use globaldata
  use int8vector_class
  implicit none

  type( int8vector_ ) :: obj, obj2
  integer( i4b ) :: m, n
  integer( i4b ), allocatable :: z( : )

  !<- interface1
  obj = Int8Vector( 10 )
  z = obj % getshape( )
  write( *, * ) z
  call obj % display( )

  !<- interface2
  obj = Int8Vector( INT( [1, 2, 4], int8 ) )
  call obj % display( )

  !<- interface3
  obj2 = Int8Vector( obj )
  call obj2 % display( )

  print *, "Real64"
  obj = Int8Vector( [1.0,2.0,3.0,4.0,5.0,6.0] )
  call obj % display( )

end program main
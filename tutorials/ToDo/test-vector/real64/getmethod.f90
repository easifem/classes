program main
  use globaldata
  use io
  use int8vector_class
  use Real64vector_class
  implicit none

  type( int8vector_ ) :: IntObj
  type( real64vector_ ) :: obj, obj2
  integer( i4b ) :: m, n
  integer( int8 ), allocatable :: z( : )
  real( real64 ), allocatable :: x( : )

  IntObj = Int8Vector( INT( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], int8 ) )

  Obj = Real64Vector( REAL( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], Real64 ) )
  call Obj % display( )

  z = ArrayValues( IntObj, TypeInt )
  IntObj = Int8Vector( z )
  call IntObj % display( )

  obj2 = ArrayValues( obj, TypeReal64Vector )
  call obj2 % display( )

  ! z = ArrayValues( IntObj, [1,2], TypeInt )
  ! IntObj = Int8Vector( z )
  ! call IntObj % display( )

  ! obj2 = ArrayValues( IntObj, [1,2], TypeInt8Vector )
  ! call obj2 % display( )


end program main
! in this program we make array of pointers

program main
  implicit none

  type :: Vector_
    REAL, ALLOCATABLE :: Val( : )
  end type

  type :: VectorPointer_
    CLASS( Vector_ ), POINTER :: Ptr
  end type

  type( VectorPointer_ ), allocatable :: PtrVec( : ), Dummy( : )

  class( Vector_ ), pointer :: obj

  real, allocatable, target :: x( : ), y( : ), z( : )

  integer :: m = 20000, n = 40000, o = 80000, i

  allocate( x( m ), y( n ), z( o ) )

  call RANDOM_NUMBER( x )
  call RANDOM_NUMBER( y )
  call RANDOM_NUMBER( z )

  allocate( PtrVec( 3 ) )

  ALLOCATE( obj )
  obj % Val = x

  PtrVec( 1 ) % Ptr => obj

  ALLOCATE( obj )
  obj % Val = y
  PtrVec( 2 ) % Ptr => obj

  ALLOCATE( obj )
  obj % Val = z
  PtrVec( 3 ) % Ptr => obj

  Dummy = PtrVec( 1 : 2 )
  DEALLOCATE( PtrVec )
  CALL MOVE_ALLOC( FROM = Dummy, TO = PtrVec )


end program main
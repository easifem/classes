program main
  use globaldata
  use genericlist_class
  implicit none

  type :: data_
    real :: x
  end type data_

  type :: datapointer_
    type( data_ ), pointer :: ptr => NULL( )
  end type datapointer_

  type( genericlist_ ) :: obj
  type( datapointer_ ) :: ptr

  ! make data type
  allocate( ptr % ptr )
  ptr % ptr % x = 2.0

  call obj % initiate( transfer( ptr, genericlistdata ) ) 
  write( *, * ) "ptr % ptr :: ", ptr % ptr

  allocate( ptr % ptr )
  ptr % ptr % x = 3.0
  call obj % InsertNewNode( transfer( ptr, genericlistdata ) ) 

  ptr = transfer( getGenericListData( obj % getNextNode( ) ), ptr )
  write( *, * ) ptr%ptr%x

end program main
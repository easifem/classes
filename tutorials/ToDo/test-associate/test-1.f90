program main
  implicit none

  type :: element_
    integer, allocatable :: nptrs( : )
    integer:: elementTopology
  end type element_

  type :: mesh_
    type( element_ ), allocatable :: elements( : )
  end type mesh_

  type( element_ ) :: obj
  type( mesh_ ) :: mesh

  obj = element_( [1,2,3], 3 )
  allocate( mesh % elements( 3 ) )
  associate( nptrs => obj % nptrs, element=>mesh % elements( 1 ) )
    write( *, * ) nptrs
    element % nptrs = nptrs
  end associate

end program main
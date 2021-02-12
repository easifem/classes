! This example shows how to remove an element to mesh.
! extraoptions  | comments
! 0 | nullify only
! 1 | nullify 
PROGRAM MAIN
  use basetype
  use basemethod
  use fe
  use mesh_class
  implicit none
  type( mesh_ ) :: obj
  integer( i4b) :: telements
  class( referenceelement_ ), pointer :: refelem => null( )
  class( element_ ), pointer :: elem => null( )
  integer( i4b ), allocatable :: nptrs( : )
  !
  telements = 4
  call initiate( obj, 2, telements )
  
  !elem-1
  refelem => referenceLine_pointer( nsd = 2 )
  allocate( elem )
  call initiate( elem, [1,2], 1, refelem )
  call Append( obj, elem )
  !elem-2
  refelem => referenceTriangle_pointer( nsd = 2 )
  allocate( elem )
  call initiate( elem, [1,2,3], 1, refelem )
  call Append( obj, elem )
  !elem-3
  refelem => referenceTriangle_pointer( nsd = 2 )
  allocate( elem )
  call initiate( elem, [4,5,6], 1, refelem )
  call Append( obj, elem )
  !elem-4
  refelem => referenceTriangle_pointer( nsd = 2 )
  allocate( elem )
  call initiate( elem, [7,8,9], 1, refelem )
  call Append( obj, elem )
  call display( obj, "mesh" )

  call removeElement( obj, iel =4, extraoption=3 )
  call display( obj, "reduced mesh" )

  call getNptrs( obj, nptrs )
  call display( nptrs, "nptrs" )
  
END PROGRAM MAIN

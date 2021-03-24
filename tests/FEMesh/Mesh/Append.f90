! This example shows how to add an element to mesh.
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
  telements = 2
  call initiate( obj, 2, telements )
  !
  refelem => referenceLine_pointer( nsd = 2 )
  allocate( elem )
  call initiate( elem, [1,2], 1, refelem )
  call Append( obj, elem )
  refelem => referenceTriangle_pointer( nsd = 2 )
  allocate( elem )
  call initiate( elem, [1,2,3], 1, refelem )
  call Append( obj, elem )
  !
  refelem => referenceTriangle_pointer( nsd = 2 )
  allocate( elem )
  call initiate( elem, [4,5,6], 1, refelem )
  call Append( obj, elem )
  !
  refelem => referenceTriangle_pointer( nsd = 2 )
  allocate( elem )
  call initiate( elem, [7,8,9], 1, refelem )
  call Append( obj, elem )
  call display( obj, "mesh" )
  !
  write( *, * ) "tElements = ", Obj % tElements
  write( *, * ) "maxElements = ", Obj %  maxElements
  !
  elem => ElementPointer( Obj = Obj, iel = 1 )
  call display( elem, "elem(1)" )
END PROGRAM MAIN

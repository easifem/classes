! In this program we show how to obtain facet mesh
!
program main
use easifem
implicit none

  type( mesh_ ) :: meshobj, facemesh
  type( msh4_ ) :: mshobj
  type( meshdata_ ) :: mdobj

  call mshobj % initiate( "./", "mesh", ".msh" , 2 )

  call mshobj % getelements( meshobj = meshobj, xidim = 2, &
    & feobj = typeelement, &
    & tagNames = [ string( "Omega_l") ] )

  ! read( *, * )
  ! call display( meshobj, 'mesh obj')
  call mdobj % initiate( meshobj )
  call mdobj % initiateBoundaryData( meshobj )
  call meshobj % getFacetElements( mdobj, faceMesh, typeFacetElement )
  call display( faceMesh, 'Face mesh obj' )

end program main

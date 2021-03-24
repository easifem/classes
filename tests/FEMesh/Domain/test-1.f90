program main
use easifem

type( domain_ ) :: obj
type( msh4_ ) :: mshobj
type( vtk_ ) :: vtu

call mshobj % initiate( path = "./", filename = "mesh", &
  & extension = ".msh", nsd = 2 )
call initiate( obj, mshobj )

call display( obj % mdomega( 1 ) % ptr % nptrs, 'nptrs' )
call display( obj % nodes, 'nodes' )

! step -1
call vtu % initiate( &
    & path = "./", &
    & filename = "test", &
    & extension = ".vtu", &
    & fmt = "ascii", &
    & meshTopology = "UnstructuredGrid", &
    & indx = [1] )

! step-2
! call vtu % startWriteGeometry( )
! call vtu % writeGeometry( MeshObj = obj % omega( 1 ) % ptr, &
!     & nodes = obj % nodes )
call vtu % writeGeometry( MeshObj = obj % omega( 1 ) % ptr, &
    & mdobj = obj % mdomega( 1 ) % ptr, &
    & nodes = obj % nodes )

! call vtu % stopWriteGeometry( )
! ! step-3
! call vtu % startWriteNodeData( )
! call vtu % writeNodeData( x = y, dofobj = dof_y )
! call vtu % stopWriteNodeData( )

! call vtu % startWriteElementData( )
! call vtu % stopWriteElementData( )

call vtu % Finalize( )

end program main
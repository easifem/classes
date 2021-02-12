program main
use easifem
implicit none

type( vtk_ ) :: obj
type( msh4_ ) :: mshFile
type( mesh_ ) :: omega
type( meshdata_ ) :: mdobj
real( dfp ), allocatable :: nodes( :, : ), y( : )
type( dof_ ) :: dof_y
integer( i4b ) :: ierr, tnodes

call mshFile % initiate( "./", "mesh", ".msh", 2 )
call mshFile % getElements( &
    & meshobj = omega, &
    & xiDim = 2, &
    & FEobj = TypeElement )

call mshFile % getNodes( Nodes = nodes )
tnodes =size( nodes, 2 )
call initiate( obj = dof_y, tnodes = [tnodes], &
    names = ["y"], spaceCompo = [2], timeCompo=[1], storageFMT = Nodes_FMT )
call initiate( y, dof_y )
call random_number( y ); y = 10.0*y

! step -1
call obj % initiate( &
    & path = "./", &
    & filename = "test", &
    & extension = ".vtu", &
    & fmt = "ascii", &
    & meshTopology = "UnstructuredGrid", &
    & indx = [1] )

! step-2
call obj % writeGeometry( MeshObj = omega, &
    & mdobj = mdobj, &
    & nodes = nodes )

! step-3
call obj % startWriteNodeData( )
call obj % writeNodeData( x = y, dofobj = dof_y )
call obj % stopWriteNodeData( )
! call obj % startWriteElementData( )
! call obj % stopWriteElementData( )

call obj % Finalize( )

end program main

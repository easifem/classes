program main
use easifem
implicit none

type( msh4_ ) :: mshobj
type( mesh_ ) :: meshobj
type( meshdata_ ) :: mdobj
type( sparsematrix_ ) :: mat

call mshobj % initiate( "./", "mesh", ".msh" , 2 )
call mshobj % getelements( meshObj = meshobj, &
  & xiDim = 2, FEObj = TypeElement )
call mdobj % initiate( meshobj )
call initiate( mat, tdof = 2, tnodes = [mdobj % tnodes] )
call setSparsity( mdobj, meshobj, mat )
call display( mat, "sparse matrix" )


end program main
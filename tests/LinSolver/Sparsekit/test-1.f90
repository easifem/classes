!<--- test of set dirichlet boundary nodes
program main
  use easifem
  implicit none

  type( sparsematrix_ ) :: tanmat
  real( dfp ), allocatable :: mat( :, : ), sol( : ), rhs( : )
  type( sparsekit_ ) :: obj
  INTEGER( I4B ) :: maxiter
  REAL( DFP ) :: tol

    call display( __LINE__, __FILE__ // " LINE :: ")
    !<--- make tanmat
    ! make csr matrix
    call initiate( obj = tanmat, tdof = 1, tnodes = [10] )
    call setsparsity( obj = tanmat, row = 1, col = [1,2] )
    call setsparsity( obj = tanmat, row = 2, col = [1,2,3] )
    call setsparsity( obj = tanmat, row = 3, col = [2,3,4] )
    call setsparsity( obj = tanmat, row = 4, col = [3,4,5] )
    call setsparsity( obj = tanmat, row = 5, col = [4,5,6] )
    call setsparsity( obj = tanmat, row = 6, col = [5,6,7] )
    call setsparsity( obj = tanmat, row = 7, col = [6,7,8] )
    call setsparsity( obj = tanmat, row = 8, col = [7,8,9] )
    call setsparsity( obj = tanmat, row = 9, col = [8,9,10] )
    call setsparsity( obj = tanmat, row = 10, col = [9,10] )
    call setsparsity( tanmat )
    call display( __LINE__, __FILE__ // " LINE :: ")
    allocate( mat( 2, 2 ) )
    mat = reshape( [1,-1,-1,1], [2,2])
    call addcontribution( tanmat, [1,2], mat, 1.0_dfp, NONE)
    call addcontribution( tanmat, [2,3], mat, 1.0_dfp, NONE)
    call addcontribution( tanmat, [3,4], mat, 1.0_dfp, NONE)
    call addcontribution( tanmat, [4,5], mat, 1.0_dfp, NONE)
    call addcontribution( tanmat, [5,6], mat, 1.0_dfp, NONE)
    call addcontribution( tanmat, [6,7], mat, 1.0_dfp, NONE)
    call addcontribution( tanmat, [7,8], mat, 1.0_dfp, NONE)
    call addcontribution( tanmat, [8,9], mat, 1.0_dfp, NONE)
    call addcontribution( tanmat, [9,10], mat, 1.0_dfp, NONE)
    call display( __LINE__, __FILE__ // " LINE :: ")
    call convert( from=tanmat, to=mat )
    allocate( rhs( 10 ), sol( 10 ) ); rhs = 0; sol = 0; sol( 10 ) = 0.5

  !<---
  tol = 1.0e-5
  maxiter = 10

  call display( __LINE__, __FILE__ // " :: ")
  call obj%initiate( &
    & Solvername = lis_cg, &
    & Maxiter = maxiter, &
    & Tol = tol )
  call display( __LINE__, __FILE__ // " :: ")
  call obj%setPrecondition( precondType = p_ilud, fpar=[1.0D-3], ipar=[0])
  call display( __LINE__, __FILE__ // " :: ")
  call obj%setsparsity( tanmat )
  call display( __LINE__, __FILE__ // " :: ")
  call obj%setDirichletBCNodes( Nptrs = [1,10], dofs=[1] )
  call display( __LINE__, __FILE__ // " :: ")
  CALL obj%setMatrix(tanmat)
  call display( __LINE__, __FILE__ // " :: ")
  call obj%solve( rhs = rhs, sol = sol )
  call display( __LINE__, __FILE__ // " :: " )
  call display( sol, 'sol', filename='sol', path='./', extension='.hdf5' )

  end program main
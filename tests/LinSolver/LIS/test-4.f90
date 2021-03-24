!<--- test of set dirichlet boundary nodes
program main
use easifem
implicit none

#include "lisf.h"

type( sparsematrix_ ) :: tanmat
real( dfp ), allocatable :: mat( :, : ), sol( : ), rhs( : )
type( lis_ ) :: obj
INTEGER( I4B ) :: maxiter
REAL( DFP ) :: tol

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
call display( 'flag4' )
call convert( from = tanmat, to = mat )
call display( 'flag5' )
call display( mat, 'mat' )
call display( 'flag6' )
allocate( rhs( 10 ), sol( 10 ) ); rhs = 0; sol = 0; sol( 10 ) = 0.5

!<---
tol = 1.0e-5
maxiter = 10

call initiate( &
  & obj = obj, &
  & Solvername = lis_cg, &
  & Maxiter = maxiter, &
  & Tol = tol )

call setPrecondition( &
  & obj = obj, &
  & precondType = p_iluk, &
  & ipar = [0]  )

call setsparsity( From = tanmat, To = obj )
call setDirichletBCNodes( Obj = obj, Nptrs = [1,10], dofs=[1] )
call convert( from = tanmat, to = obj )
call solve( obj = obj, rhs = rhs, sol = sol )
call display( sol, 'sol' )

end program main
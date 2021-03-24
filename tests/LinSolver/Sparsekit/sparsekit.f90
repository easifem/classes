! incomplete lu factorization
program main
  use easifem
  implicit none

  type( sparsematrix_ ) :: tanmat
  real( dfp ), allocatable :: mat( :, : )
  type( sparsekit_ ) :: obj
  type( IterationData_ ) :: Iterdata
  real( dfp ) :: sol0( 10 ), sol( 10 ), rhs( 10 )

  call initiate( obj = tanmat, tdof = 1, tnodes = [10] )

  call display( 'flag-1' )

  call setsparsity( obj = tanmat, row = 1, col = [1,2] )

  call display( 'flag-2' )

  call setsparsity( obj = tanmat, row = 2, col = [1,2,3] )
  call setsparsity( obj = tanmat, row = 3, col = [2,3,4] )
  call setsparsity( obj = tanmat, row = 4, col = [3,4,5] )
  call setsparsity( obj = tanmat, row = 5, col = [4,5,6] )
  call setsparsity( obj = tanmat, row = 6, col = [5,6,7] )
  call setsparsity( obj = tanmat, row = 7, col = [6,7,8] )
  call setsparsity( obj = tanmat, row = 8, col = [7,8,9] )
  call setsparsity( obj = tanmat, row = 9, col = [8,9,10] )
  call setsparsity( obj = tanmat, row = 10, col = [9,10] )
  call setsparsity( tanmat ) !<-- some final touch up


  stop


  allocate( mat( 2, 2 ) )
  mat = reshape( [1,-1,-1,1], [2,2])
  call addcontribution( tanmat, [1,2], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [2,3], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [3,4], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [4,5], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [5,6], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [6,7], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [7,8], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [8,9], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [9,10], mat, 1.0_dfp, NONE )

  call convert( from = tanmat, to = mat )
  ! call spy( tanmat, [String( './' ), String( 'tanmat' ), String( '.txt' )] &
  !   & , 'gnuplot', .false. )
  call display( mat, 'mat' )

  CALL initiate( Obj  = Iterdata, &
      & Tolerance = 1.0d-6, &
      & ConvergenceType = RelativeConvergence, &
      & ConvergenceIn = ConvergenceInRes, &
      & MaxIter = 18 )

  call display( 'flag-1' )
  call initiate( obj = obj, solvername = lis_cg, iterdata = iterdata, &
    & tInit = 5, Krylovsize = 15 )

  call display( 'flag-2' )
  call setPreconditioning( obj = obj, precondSide = precondRight, &
    & precondType = p_ilud, droptol = 1.0d-2, permtol = 0.1_dfp, lfil = 5, &
    & mbloc = 0, alpha = 1.0_dfp )

  call display( 'flag-3' )
  call setsparsity( To = obj, from = tanmat )

  call display( 'flag-4' )
  call setDirichletBCNodes( obj = obj, nptrs = [1, 10], dofs = [1] )

  call display( 'flag-5' )
  call convert( from = tanmat, to = obj )
  call display( obj % ierr, 'ierr')

  rhs = 0.0; sol = 1.0; sol( 1 ) = 0.0; sol( 10 ) = 0.5
  call solve( obj = obj, sol = sol, rhs = rhs )
  call display( rhs, 'rhs' )
  call display( sol, 'sol' )

  ! block
  !   INTEGER( I4B ) :: its, i, j, a, b, tdbnptrs
  !   REAL( DFP ) :: val

  !   ! applying dbc
  !   IF( ALLOCATED( Obj % dbcnptrs ) ) THEN
  !     call display( 'flag-1')
  !     tdbnptrs = SIZE( Obj % dbcnptrs )
  !     DO j = 1, tdbnptrs
  !       val = sol( Obj % dbcnptrs( j ) )

  !       DO i = Obj % dbcindx( j ), Obj % dbcindx( j + 1 ) - 1
  !         rhs( Obj % dbcIA( i ) ) = rhs( Obj % dbcIA( i ) ) &
  !           & - Obj % A( Obj % dbcJA( i ) ) * val
  !         IF( Obj % dbcnptrs( j ) .EQ. Obj % dbcIA( i ) ) THEN
  !           Obj % A( Obj % dbcJA( i ) ) = 1.0_DFP
  !         ELSE
  !           Obj % A( Obj % dbcJA( i ) ) = 0.0_DFP
  !         END IF
  !       END DO
  !       rhs( Obj % dbcnptrs( j ) ) = val
  !     END DO

  !     call convert( Obj % A, Obj % IA, Obj % JA, mat )
  !     call display( mat, 'mat' )
  !     call display( 'flag-2' )
  !     call display( rhs, 'rhs' )

  !     DO i = 1, tdbnptrs
  !       a = Obj % IA( Obj % dbcnptrs( i ) )
  !       b = Obj % IA( Obj % dbcnptrs( i ) + 1 ) - 1
  !       DO j = a, b
  !         IF( Obj % JA( j ) .EQ. Obj % dbcnptrs( i ) ) THEN
  !           Obj % A( j ) = 1.0_DFP
  !         ELSE
  !           Obj % A( j ) = 0.0_DFP
  !         END IF
  !       END DO
  !     END DO

  !     call convert( Obj % A, Obj % IA, Obj % JA, mat )
  !     call display( mat, 'mat' )

  !   END IF
  ! end block

  ! block
  !   type( intvector_ ) :: intvec( 3 )

  !   intvec( 1 ) % val = [1, 5]
  !   call setDirichletBCNodes( obj = obj, nptrs = intvec, dofs = [1] )

  !   call display( obj % dbcnptrs, 'dbc nodes' )
  !   call display( obj % dbcJA, 'dbc JA' )
  !   call display( obj % dbcindx, 'dbc indx' )
  !   end block

  end program main
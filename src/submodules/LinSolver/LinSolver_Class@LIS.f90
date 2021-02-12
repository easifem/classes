SUBMODULE( LinSolver_Class ) LIS
USE BaseMethod
IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE lis_initiate
  CHARACTER( LEN = 100 ) :: opt

  Obj % SolverName = SolverName
  Obj % ierr = 0

  call lis_vector_create( Obj % comm, Obj % lis_res, Obj % ierr )
  call chkerr( Obj % ierr )

  call lis_vector_create( Obj % comm, Obj % lis_rhs, Obj % ierr )
  call chkerr( Obj % ierr )

  call lis_vector_create( Obj % comm, Obj % lis_sol, Obj % ierr )
  call chkerr( Obj % ierr )

  call lis_matrix_create( Obj % comm, Obj % lis_mat, Obj % ierr )
  call chkerr( Obj % ierr )

  call lis_solver_create( Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

  call lis_precon_create( Obj % lis_solver, obj % lis_precon, Obj % ierr )
  call chkerr( Obj % ierr )

  SELECT CASE( SolverName )
  CASE( lis_bicgstabl )
    IF( PRESENT( ipar ) ) THEN
    opt = '-i bicgstabl -ell ' &
      & // TRIM( str( n = ipar( 1 ), no_sign = .true. ) )
    ELSE
      opt = '-i bicgstabl'
    END IF

  CASE( lis_orthomin, lis_gmres, lis_fgmres )
    IF( PRESENT( ipar ) ) THEN
    opt = '-i ' // TRIM( INT2STR( Obj % SolverName ) ) &
      & // ' -restart ' &
      & // TRIM( str( n = ipar( 1 ), no_sign = .true.) )
    ELSE
      opt = '-i ' // TRIM( INT2STR( Obj % SolverName ) )
    END IF

  CASE ( lis_idrs )
    IF( PRESENT( ipar ) ) THEN
      opt = '-i idrs -irestart ' // TRIM( INT2STR( ipar( 1 ) ) )
    ELSE
      opt = '-i idrs'
    END IF

  CASE ( lis_sor )
    IF( PRESENT( fpar ) ) THEN
      opt = '-i sor -omega ' &
        & // TRIM( str( n = fpar( 1 ), no_sign = .true., compact = .true. ) )
    ELSE
      opt = '-i sor '
    END IF

  CASE DEFAULT
    opt = '-i '// TRIM( INT2STR( Obj % SolverName ) )
  END SELECT

  !<--- setting up solver name
  call display( opt )
  call lis_solver_set_option( TRIM( opt ), Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

  !<--- max iteration
  opt = '-maxiter '//TRIM( INT2STR( MaxIter ) )
  call display( opt )
  call lis_solver_set_option( TRIM( opt ), Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

  !<--- print residual detials on terminal
  opt = '-print 3'
  call display( opt )
  call lis_solver_set_option( TRIM( opt ), Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

  !<--- Diagonal  scaling
  IF( PRESENT( DiagScale ) ) THEN
    opt = '-scale '//TRIM( INT2STR( DiagScale ) )
  ELSE
    opt = '-scale none'
  END IF

  !<--- diagonal scaling
  call display( opt )
  call lis_solver_set_option( TRIM( opt ), Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

  !<--- setting up tolerance
  opt = '-tol '// TRIM( str( n = Tol, no_sign = .true., compact=.true. ) )
  call display( opt )
  call lis_solver_set_option( TRIM( opt ), Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

  !<-- behavior of initial guess
  opt = '-initx_zeros false'
  call display( opt )
  call lis_solver_set_option( TRIM( opt ), Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

  !<--- convergence criteria
  opt = '-conv_cond nrm2_r'
  call display( opt )
  call lis_solver_set_option( TRIM( opt ), Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

END PROCEDURE lis_initiate

!----------------------------------------------------------------------------
!                                                            setPrecondition
!----------------------------------------------------------------------------

MODULE PROCEDURE lis_setprecond
  CHARACTER( LEN = 100 ) :: opt

  Obj % precondtype = precondtype

  SELECT CASE( Obj % precondtype )
  CASE( p_none )
    opt = '-p none'
  CASE( p_jacobi )
    opt = '-p jacobi'
  CASE( p_iluk )
    IF( PRESENT( ipar ) ) THEN
      opt = '-p ilu -ilu_fill '// TRIM( INT2STR( ipar( 1 ) ) )
    ELSE
      opt = '-p ilu'
    END IF
  CASE( p_ssor )
    IF( PRESENT( fpar ) ) THEN
      opt = '-p ssor -ssor_omega ' &
        & // TRIM( str( n = fpar( 1 ), no_sign = .true., compact=.true. ) )
    ELSE
      opt = '-p ssor'
    END IF

  CASE( p_hybrid )
    IF( PRESENT( ipar ) ) THEN
      opt = '-p hybrid -hybrid_i ' &
        & // TRIM( str( n = ipar( 1 ), no_sign = .true. ) ) &
        & // ' -hybrid_maxiter ' &
        & // TRIM( str( n = ipar( 2 ), no_sign = .true. ) ) &
        & // ' -hybrid_ell ' &
        & // TRIM( str( n = ipar( 3 ), no_sign = .true. ) ) &
        & // ' -hybrid_restart ' &
        & // TRIM( str( n = ipar( 3 ), no_sign = .true. ) )
    ELSE
      opt = '-p hybrid'
    END IF

    IF( PRESENT( fpar ) ) THEN
      opt = TRIM( opt ) &
        & // ' -hybrid_tol ' &
        & // TRIM( str( n = fpar( 1 ), no_sign = .true., compact = .true. ) ) &
        & // ' -hybrid_omega ' &
        & // TRIM( str( n = fpar( 2 ), no_sign = .true., compact = .true. ) )
    END IF

  CASE( p_is )
    IF( PRESENT( ipar ) ) THEN
      opt = '-p is ' &
        & // 'is_m ' &
        & // TRIM( str( n = ipar( 1 ), no_sign = .true. ) )
    ELSE
      opt = '-p is '
    END IF
    IF( PRESENT( fpar ) ) THEN
      opt = TRIM( opt ) &
        & // ' is_alpha ' &
        & // TRIM( str( n = fpar( 1 ), no_sign = .true., compact = .true. ) )
    END IF

  CASE( p_sainv )
    IF( PRESENT( fpar ) ) THEN
      opt = '-p sainv -sainv_drop ' &
        & // TRIM( str( n = fpar( 1 ), no_sign = .true., compact = .true. ) )
    ELSE
      opt = '-p sainv'
    END IF

  CASE( p_saamg )
    IF( PRESENT( ipar ) ) THEN
      IF( ipar( 1 ) .EQ. 1 ) THEN
        opt = '-p saamg -sammg_unsym true'
      ELSE
        opt = '-p saamg -sammg_unsym false'
      END IF
    ELSE
      opt = '-p saamg'
    END IF

    IF( PRESENT( fpar ) ) THEN
      opt = TRIM( opt ) // ' -saamg_theta ' &
        & // TRIM( str( n = fpar( 1 ), no_sign = .true. , compact = .true.) )
    END IF

  CASE( p_iluc )
    IF( PRESENT( fpar ) ) THEN
      opt = '-p iluc -iluc_drop' &
        & // TRIM( str( n = fpar( 1 ), no_sign = .true., compact = .true. ) ) &
        & // ' -iluc_rate ' &
        & // TRIM( str( n = fpar( 2 ), no_sign = .true., compact = .true. ) )
    ELSE
      opt = '-p iluc'
    END IF
  CASE( p_ilut )
    IF( PRESENT( ipar ) ) THEN
      IF( ipar( 1 ) .EQ. 1 ) THEN
        opt = '-p ilut -adds true -adds_iter ' &
          //TRIM( str( n = ipar( 1 ), no_sign = .true.) )
      END IF
    ELSE
      opt = '-p ilut'
    END IF
  END SELECT

  call display( opt )
  call lis_solver_set_option( TRIM( opt ), Obj % lis_solver, Obj % ierr )
  call chkerr( Obj % ierr )

END PROCEDURE lis_setprecond

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE lis_set_sparsity
  INTEGER( I4B ) :: nnz, nrow
  nrow = From % nrow
  nnz = From % nnz
  To % tdof = From % tdof
  IF( ALLOCATED( TO % A ) ) DEALLOCATE( TO % A )
  IF( ALLOCATED( TO % IA ) ) DEALLOCATE( TO % IA )
  IF( ALLOCATED( TO % JA ) ) DEALLOCATE( TO % JA )
  ALLOCATE( TO % A( 0 : nnz - 1 ), TO % JA( 0 : nnz - 1 ) )
  ALLOCATE( TO % IA( 0 : nrow ) )
  TO % A( 0 :  ) = 0.0_DFP
  TO % IA( 0 : ) = From % IA - 1
  TO % JA( 0 : ) = From % JA - 1
  To % MatrixProp = From % MatrixProp

  call lis_vector_set_size( To % lis_rhs, 0, nrow, To % ierr )
  call chkerr( To % ierr )

  call lis_vector_set_all( 0.0_dfp, To % lis_rhs, To % ierr )
  call chkerr( To % ierr )

  call lis_vector_set_size( To % lis_sol, 0, nrow, To % ierr )
  call chkerr( To % ierr )

  call lis_vector_set_all( 0.0_dfp, To % lis_sol, To % ierr )
  call chkerr( To % ierr )

  call lis_matrix_set_size( To % lis_mat, 0, nrow, To % ierr )
  call chkerr( To % ierr )

  call lis_matrix_set_csr( nnz, To % IA,  To % JA, To % A, To % lis_mat, &
    & To % ierr )
  call chkerr( To % ierr )

END PROCEDURE lis_set_sparsity

!----------------------------------------------------------------------------
!                                                         setDirichletBCNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE lis_setdbc_1
  INTEGER( I4B ) :: n, m, a, b, idof, nrow, i, j, tdbnptrs
  LOGICAL( LGT ), ALLOCATABLE :: dbcmask( : )
  type( intvector_ ), ALLOCATABLE :: intvec( : )
  INTEGER( i4b ) :: count0
  INTEGER( I4B ), ALLOCATABLE :: RowSize( : ), ColSize( : )

  nrow = SIZE( Obj % IA ) - 1
  n = size( nptrs )
  m = size( dofs )
  tdbnptrs = m * n
  !
  ! CALL reallocate( obj % dbcnptrs,  tdbnptrs, ColSize, nrow, Rowsize, nrow )
  IF( ALLOCATED( Obj % dbcnptrs ) ) DEALLOCATE( Obj % dbcnptrs )
  ALLOCATE( Obj % dbcnptrs( 0 : tdbnptrs -1 ) )

  ALLOCATE( ColSize( 0:nrow - 1 ), RowSize( 0 : nrow - 1 ) )
  !
  a = 0; b = 0;
  DO idof = 1, m
    b = idof * n - 1
    obj % dbcnptrs( a : b ) = ( nptrs - 1 ) * obj % tdof + dofs( idof )
    a = b + 1;
  END DO
  !
  DO i = 0, tdbnptrs-1
    obj % dbcnptrs( i ) = obj % dbcnptrs( i ) - 1
  END DO
  !
  DO i = 0, nrow-1
    a = Obj % IA( i )
    b = Obj % IA( i + 1 ) - 1
    DO j = a, b
      ColSize( Obj % JA ( j ) ) = ColSize( Obj % JA ( j ) ) + 1
    END DO
  END DO
  !
  ALLOCATE( dbcmask( 0 : nrow-1 ) )
  dbcmask( 0 : ) = .FALSE.
  dbcmask( obj % dbcnptrs ) = .TRUE.
  !
  count0 = 0;
  !
  DO i = 0, nrow-1
    IF( dbcmask( i ) ) THEN
      Obj % dbcnptrs( count0 ) = i
      RowSize( i ) = count0
      count0 = count0 + 1;
    END IF
  END DO
  !
  allocate( intvec( 0 : tdbnptrs-1 )  )
  a = 0; b = 0;
  DO i = 0, nrow-1
    DO j = Obj % IA( i ), Obj % IA( i + 1 ) - 1
      a = Obj % JA( j )
      IF( dbcmask( a ) ) THEN
        call append( Intvec( RowSize( a ) ), [j, i] )
        b = b + 1
      END IF
    END DO
  END DO
  !
  if( allocated( obj % dbcJA ) ) deallocate( obj % dbcJA )
  allocate( obj % dbcJA( 0 : b -1 ) )
  obj % dbcJA( 0 : ) = 0

  if( allocated( obj % dbcIA ) ) deallocate( obj % dbcIA )
  allocate( obj % dbcIA( 0 : b -1 ) )
  obj % dbcIA( 0 : ) = 0

  if( allocated( obj % dbcIndx ) ) deallocate( obj % dbcIndx )
  allocate( obj % dbcIndx( 0 : tdbnptrs ) )
  obj % dbcIndx( 0 : ) = 0

  a = 0; b = 0
  DO i = 0, tdbnptrs-1
    m = SIZE( intvec( i ) % Val )
    b = b - 1 + m/2
    obj % dbcindx( i ) = a
    IF( m .eq. 0 ) cycle
    obj % dbcJA( a : b ) = intvec( i ) % Val( 1 : m : 2 )
    obj % dbcIA( a : b ) = intvec( i ) % Val( 2 : m : 2 )
    a = b + 1;
  END DO
  obj % dbcindx( tdbnptrs ) = SIZE( obj % dbcJA )
  !
  Deallocate( dbcmask, intvec, Rowsize, ColSize )
END PROCEDURE lis_setdbc_1

!----------------------------------------------------------------------------
!                                                         setDirichletBCNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE lis_setdbc_2
  INTEGER( I4B ) :: m, a, b, idof, nrow, i, j, tdbnptrs
  LOGICAL( LGT ), ALLOCATABLE :: dbcmask( : )
  type( intvector_ ), ALLOCATABLE :: intvec( : )
  INTEGER( i4b ) :: count0
  INTEGER( I4B ), ALLOCATABLE :: RowSize( : ), ColSize( : )

  nrow = SIZE( Obj % IA ) - 1
  m = size( dofs )
  tdbnptrs = 0
  DO i = 1, m
    tdbnptrs = tdbnptrs + SIZE( Nptrs( i ) % Val )
  END DO
  !
  ! CALL reallocate( obj % dbcnptrs,  tdbnptrs, ColSize, nrow, Rowsize, nrow )
  IF( ALLOCATED( Obj % dbcnptrs ) ) DEALLOCATE( Obj % dbcnptrs )
  ALLOCATE( Obj % dbcnptrs( 0 : tdbnptrs -1 ) )

  ALLOCATE( ColSize( 0:nrow - 1 ), RowSize( 0 : nrow - 1 ) )
  !
  a = 0; b = 0;
  DO idof = 1, m
    b = a +  SIZE( Nptrs( idof ) % Val ) - 1
    obj % dbcnptrs( a : b ) = &
      & ( Nptrs( idof ) % Val - 1 ) * obj % tdof + dofs( idof )
    a = b + 1;
  END DO
  !
  DO i = 0, tdbnptrs-1
    obj % dbcnptrs( i ) = obj % dbcnptrs( i ) - 1
  END DO
  !
  DO i = 0, nrow-1
    a = Obj % IA( i )
    b = Obj % IA( i + 1 ) - 1
    DO j = a, b
      ColSize( Obj % JA ( j ) ) = ColSize( Obj % JA ( j ) ) + 1
    END DO
  END DO
  !
  ALLOCATE( dbcmask( 0 : nrow-1 ) )
  dbcmask( 0 : ) = .FALSE.
  dbcmask( obj % dbcnptrs ) = .TRUE.
  !
  count0 = 0;
  !
  DO i = 0, nrow-1
    IF( dbcmask( i ) ) THEN
      Obj % dbcnptrs( count0 ) = i
      RowSize( i ) = count0
      count0 = count0 + 1;
    END IF
  END DO
  !
  allocate( intvec( 0 : tdbnptrs-1 )  )
  a = 0; b = 0;
  DO i = 0, nrow-1
    DO j = Obj % IA( i ), Obj % IA( i + 1 ) - 1
      a = Obj % JA( j )
      IF( dbcmask( a ) ) THEN
        call append( Intvec( RowSize( a ) ), [j, i] )
        b = b + 1
      END IF
    END DO
  END DO
  !
  if( allocated( obj % dbcJA ) ) deallocate( obj % dbcJA )
  allocate( obj % dbcJA( 0 : b -1 ) )
  obj % dbcJA( 0 : ) = 0

  if( allocated( obj % dbcIA ) ) deallocate( obj % dbcIA )
  allocate( obj % dbcIA( 0 : b -1 ) )
  obj % dbcIA( 0 : ) = 0

  if( allocated( obj % dbcIndx ) ) deallocate( obj % dbcIndx )
  allocate( obj % dbcIndx( 0 : tdbnptrs ) )
  obj % dbcIndx( 0 : ) = 0

  a = 0; b = 0
  DO i = 0, tdbnptrs-1
    m = SIZE( intvec( i ) % Val )
    b = b - 1 + m/2
    obj % dbcindx( i ) = a
    IF( m .eq. 0 ) cycle
    obj % dbcJA( a : b ) = intvec( i ) % Val( 1 : m : 2 )
    obj % dbcIA( a : b ) = intvec( i ) % Val( 2 : m : 2 )
    a = b + 1;
  END DO
  obj % dbcindx( tdbnptrs ) = SIZE( obj % dbcJA )
  !
  Deallocate( dbcmask, intvec, Rowsize, ColSize )
END PROCEDURE lis_setdbc_2

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE lis_setmatrix
  INTEGER( I4B ) :: nnz, nrow, ncol, indx, i, m, j, iwk, fac
  To % A( 0 : ) = From % A( 1 : )
  call lis_matrix_assemble( To % lis_mat, To % ierr )
  call chkerr( To % ierr )
END PROCEDURE lis_setmatrix

!----------------------------------------------------------------------------
!                                                                     Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE lis_solve_1
  INTEGER( I4B ) :: j, i, tdbnptrs, a, b
  REAL( DFP ) :: val

  ! applying dbc
  IF( ALLOCATED( Obj % dbcnptrs ) ) THEN
    tdbnptrs = SIZE( Obj % dbcnptrs )
    DO j = 0, tdbnptrs-1
      val = sol( Obj % dbcnptrs( j ) + 1 )

      DO i = Obj % dbcindx( j ), Obj % dbcindx( j + 1 ) - 1

        rhs( Obj % dbcIA( i ) + 1 ) = &
          & rhs( Obj % dbcIA( i ) + 1 ) &
          & - Obj % A( Obj % dbcJA( i ) ) * val

        IF( Obj % dbcnptrs( j ) .EQ. Obj % dbcIA( i ) ) THEN
          Obj % A( Obj % dbcJA( i ) ) = 1.0_DFP
        ELSE
          Obj % A( Obj % dbcJA( i ) ) = 0.0_DFP
        END IF

      END DO

    END DO

    DO i = 0, tdbnptrs - 1
      rhs( Obj % dbcnptrs( i ) + 1 ) = sol( Obj % dbcnptrs( i ) + 1 )
    END DO

    DO i = 0, tdbnptrs-1
      a = Obj % IA( Obj % dbcnptrs( i ) )
      b = Obj % IA( Obj % dbcnptrs( i ) + 1 ) - 1
      DO j = a, b
        IF( Obj % JA( j ) .EQ. Obj % dbcnptrs( i ) ) THEN
          Obj % A( j ) = 1.0_DFP
        ELSE
          Obj % A( j ) = 0.0_DFP
        END IF
      END DO
    END DO
  END IF

  ! now set values in vector
  tdbnptrs = SIZE( rhs )
  DO i = 1, tdbnptrs

    call lis_vector_set_value( LIS_INS_VALUE, i, rhs( i ), Obj % lis_rhs, &
      & Obj % ierr )
    call chkerr( Obj % ierr )

    call lis_vector_set_value( LIS_INS_VALUE, i, sol( i ), Obj % lis_sol, &
      & Obj % ierr )
    call chkerr( Obj % ierr )

  END DO

  call lis_solve( Obj % lis_mat, Obj % lis_rhs, Obj % lis_sol, &
    & Obj % lis_solver, Obj % ierr )

  ! call lis_solve_kernel( &
  !   & Obj % lis_mat, Obj % lis_rhs, Obj % lis_sol, Obj % lis_solver, &
  !   & Obj % lis_precon, Obj % ierr )

  call chkerr( Obj % ierr )

  call lis_vector_get_values( Obj % lis_sol, 1_I4B, tdbnptrs, sol, Obj % ierr )
  call chkerr( Obj % ierr )

END PROCEDURE lis_solve_1

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! @todo
! To be implemented soon
! @endtodo

MODULE PROCEDURE lis_display
END PROCEDURE lis_display

!----------------------------------------------------------------------------
!                                                         WriteResidueHistory
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! @todo
! To be implemented soon
! @endtodo

MODULE PROCEDURE lis_write_res_his
END PROCEDURE lis_write_res_his

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! @todo
! To be implemented soon
! @endtodo

MODULE PROCEDURE lis_deallocatedata
END PROCEDURE lis_deallocatedata

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE LIS
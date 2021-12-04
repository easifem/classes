SUBMODULE(LinSolver_Class) Sparsekit
USE BaseMethod
IMPLICIT NONE

CONTAINS

!-----------------------------------------------------------------------------
!                                                                    Initiate
!-----------------------------------------------------------------------------

MODULE PROCEDURE skit_initiate

  obj % SolverName = SolverName
  obj % ierr = 0
  obj % ipar = 0
  obj % fpar = 0.0
  !
  obj % ipar( 1 ) = 0

  obj % ipar( 5 ) = 20
  SELECT CASE( SolverName )
  CASE( lis_gmres, lis_fgmres, lis_dqgmres, lis_fom )
    IF( PRESENT( ipar ) ) THEN
      obj % ipar( 5 ) = ipar( 1 )
    END IF
  END SELECT

  obj % ipar( 6 ) = MaxIter
  CALL reallocate( obj % Res, MaxIter + 1 )
  obj % ipar( 13 ) = 10
  obj % ipar( 3 )  = 1
  obj % fpar( 1 ) = Tol
  obj % fpar( 2 ) = 0.0

END PROCEDURE skit_initiate

!----------------------------------------------------------------------------
!                                                         setPreconditioning
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_setprecond
  !
  obj % precondType = precondType
  ! always left precond
  obj % ipar( 2 ) = 1
  obj % lfil = 10
  obj % mbloc = 0
  obj % droptol = 1.0E-4
  obj % permtol = 0.5
  obj % alpha = 1.0

  SELECT CASE( precondType )
  CASE( p_none )
    obj % ipar( 2 ) = 0
  CASE( p_ilut )
    ! extra options are drop-tol and lfil
    IF( PRESENT( fpar ) ) THEN
      obj % droptol = fpar( 1 )
    END IF

    IF( PRESENT( ipar ) ) THEN
      obj % lfil = ipar( 1 )
    END IF

  CASE( p_ilutp )
    ! extra option
    ! ipar( 1 ) = lfil
    ! ipar( 2 ) = mbloc
    ! fpar( 1 ) = droptol
    ! fpar( 2 ) = permtol
    IF( PRESENT( ipar ) ) THEN
      obj % lfil = ipar( 1 )
      obj % mbloc = ipar( 2 )
    END IF

    IF( PRESENT( fpar ) ) THEN
      obj % droptol = fpar( 1 )
      obj % permtol = fpar( 2 )
    END IF

  CASE( p_ilud )
    ! fpar( 1 ) = droptol
    ! fpar( 2 ) = alpha
    IF( PRESENT( fpar ) ) THEN
      obj % droptol = fpar( 1 )
      obj % alpha = fpar( 2 )
    END IF

  CASE( p_iludp )
    IF( PRESENT( ipar ) ) THEN
      obj % mbloc = ipar( 1 )
    END IF
    IF( PRESENT( fpar ) ) THEN
      obj % droptol = fpar( 1 )
      obj % alpha = fpar( 2 )
      obj % permtol = fpar( 3 )
    END IF

  END SELECT

END PROCEDURE skit_setprecond

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_set_sparsity
  INTEGER( I4B ) :: i, m, iwk, fac
  !
  To%Matrixprop = From%Matrixprop
  To%tdof = From%tdof
  CALL Reallocate( To%tNodes, From%tdof )
  To%tNodes = From %tNodes
  To%StorageFMT = From%storageFMT
  !
  To%A => From%A
  To%IA => From%IA
  To%JA => From%JA
  !
  SELECT CASE( To % SolverName )
    CASE( lis_cg ); i = 5 * From % nrow

    CASE( lis_cgnr ); i = 5 * From % nrow

    CASE( lis_bcg ); i = 7 * From % nrow

    CASE( lis_dbcg ); i = 11 * From % nrow

    CASE( lis_bcgstab ); i = 8 * From % nrow

    CASE( lis_tfqmr ); i = 11 * From % nrow

    CASE( lis_fom, lis_gmres )
      m = To % ipar( 5 )
      i = ( From % nrow + 3 ) * ( m + 2) + ( m + 1 ) * m / 2

    CASE( lis_fgmres )
      m = To % ipar( 5 )
      i = 2 * From % nrow * ( m + 1 ) + ( m + 1 ) * m / 2 + 3 * m + 2

    CASE( lis_dqgmres )
      m = To % ipar( 5 ) + 1
      i = From % nrow + m * ( 2 * From % nrow + 4 )
  END SELECT
  !
  To % ipar( 4 ) = i
  CALL reallocate( To % WK, i )
  !
  !precondition related
  IF( To % ipar( 2 ) .NE. 0 ) THEN
    fac = 3; iwk = fac * From % nnz
    CALL reallocate( To % ALU, iwk, To % JU, From % nrow, To % JLU, iwk )
    !
    SELECT CASE( To % precondType  )
    CASE( p_ilut )
      CALL reallocate( To % W, From % nrow + 1, To % JW, 2*From % nrow )

    CASE( p_ilutp )
      CALL reallocate( To % W, From % nrow + 1, To % IPERM, 2*From % nrow, &
        & To % JW, 2*From % nrow )
      IF( To % mbloc .EQ. 0 ) To % mbloc = From % nrow

    CASE( p_ilud )
      CALL reallocate( To % W, 2*From % nrow, To % JW, 2*From % nrow )

    CASE( p_iludp )
      CALL reallocate( To % W, 2*From % nrow, To % JW, 2*From % nrow, &
        & To % IPERM, 2*From % nrow )
      IF( To % mbloc .EQ. 0 ) To % mbloc = From % nrow

    END SELECT
  END IF
END PROCEDURE skit_set_sparsity

!----------------------------------------------------------------------------
!                                                         setDirichletBCNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_setdbc_1
  INTEGER( I4B ) :: n, m, a, b, idof, nrow, i, j, tdbnptrs
  LOGICAL( LGT ), ALLOCATABLE :: dbcmask( : )
  type( intvector_ ), ALLOCATABLE :: intvec( : )
  INTEGER( i4b ) :: count0
  INTEGER( I4B ), ALLOCATABLE :: RowSize( : ), ColSize( : )

  nrow = SIZE( obj % IA ) - 1
  n = size( nptrs )
  m = size( dofs )
  tdbnptrs = m * n
  !
  CALL reallocate( obj % dbcnptrs,  tdbnptrs, ColSize, nrow, Rowsize, nrow )
  !
  SELECT CASE( obj%storageFMT )
  CASE( Nodes_FMT )
    a = 0; b = 0;
    DO idof = 1, m
      a = b + 1; b = b + n
      obj%dbcnptrs( a : b ) = ( nptrs - 1 ) * obj % tdof + dofs( idof )
    END DO
  CASE( DOF_FMT )
    a = 0; b = 0;
    DO idof = 1, m
      a = b + 1
      b = b + n
      obj%dbcnptrs( a:b ) = ( dofs(idof)-1) * obj%tNodes(dofs(idof)) + nptrs
    END DO
  END SELECT
  !
  DO i = 1, nrow
    a = obj%IA( i )
    b = obj%IA( i + 1 ) - 1
    DO j = a, b
      ColSize( obj%JA ( j ) ) = ColSize( obj%JA ( j ) ) + 1
    END DO
  END DO
  !
  ALLOCATE( dbcmask(nrow) ); dbcmask = .FALSE.
  dbcmask( obj % dbcnptrs ) = .TRUE.
  count0 = 0; a = 0; b = 0
  allocate( intvec( tdbnptrs )  )
  !
  DO i = 1, nrow
    IF( dbcmask( i ) ) THEN
      count0 = count0 + 1;
      obj % dbcnptrs( count0 ) = i
      RowSize( i ) = count0
    END IF
  END DO
  !
  b = 0;
  DO i = 1, nrow
    DO j = obj % IA( i ), obj % IA( i + 1 ) - 1
      a = obj % JA( j )
      IF( dbcmask( a ) ) THEN
        b = b + 1
        call append( Intvec( RowSize( a ) ), [j, i] )
      END IF
    END DO
  END DO
  !
  call reallocate( obj % dbcJA, b,  obj % dbcIA, b, obj % dbcindx, tdbnptrs + 1 )
  a = 0; b = 0
  DO i = 1, tdbnptrs
    m = SIZE( intvec( i ) % Val )
    a = b + 1; b = b + m/2
    obj % dbcindx( i ) = a
    IF( m .eq. 0 ) cycle
    obj % dbcJA( a : b ) = intvec( i ) % Val( 1 : m : 2 )
    obj % dbcIA( a : b ) = intvec( i ) % Val( 2 : m : 2 )
  END DO
  obj % dbcindx( tdbnptrs + 1 ) = SIZE( obj % dbcJA ) + 1
  !
  Deallocate( dbcmask, intvec, Rowsize, ColSize )
END PROCEDURE skit_setdbc_1

!----------------------------------------------------------------------------
!                                                        setDirichletBCNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_setdbc_2
  INTEGER( I4B ) :: n, m, a, b, idof, nrow, i, j, tdbnptrs
  LOGICAL( LGT ), ALLOCATABLE :: dbcmask( : )
  type( intvector_ ), ALLOCATABLE :: intvec( : )
  INTEGER( i4b ) :: count0
  INTEGER( I4B ), ALLOCATABLE :: RowSize( : ), ColSize( : )

  nrow = SIZE( obj % IA ) - 1
  m = size( dofs ); tdbnptrs = 0
  DO i = 1, m
    tdbnptrs = tdbnptrs + SIZE( Nptrs( i ) )
  END DO
  !
  CALL reallocate( obj%dbcnptrs,  tdbnptrs, ColSize, nrow, Rowsize, nrow )
  !
  a = 0; b = 0;
  DO idof = 1, m
    IF( SIZE( Nptrs( idof ) ) .EQ. 0 ) CYCLE
    a = b + 1
    b = b +  SIZE( Nptrs( idof ) % Val )
    obj % dbcnptrs( a : b ) = &
      & ( Nptrs( idof ) % Val - 1 ) * obj % tdof + dofs( idof )
  END DO

  SELECT CASE( obj%storageFMT )
  CASE( Nodes_FMT )
    a = 0; b = 0;
    DO idof = 1, m
      a = b + 1; b = b + n
      obj%dbcnptrs( a : b ) = ( nptrs( idof ) % Val - 1 ) * obj % tdof &
        & + dofs( idof )
    END DO
  CASE( DOF_FMT )
    a = 0; b = 0;
    DO idof = 1, m
      a = b + 1
      b = b + n
      obj%dbcnptrs( a:b ) = ( dofs(idof)-1) * obj%tNodes(dofs(idof)) &
        & + nptrs( idof ) % Val
    END DO
  END SELECT

  !
  DO i = 1, nrow
    a = obj % IA( i )
    b = obj % IA( i + 1 ) - 1
    DO j = a, b
      ColSize( obj % JA ( j ) ) = ColSize( obj % JA ( j ) ) + 1
    END DO
  END DO
  !
  allocate( dbcmask( nrow ) ); dbcmask = .false.
  dbcmask( obj % dbcnptrs ) = .TRUE.
  count0 = 0; a = 0; b = 0
  allocate( intvec( tdbnptrs )  )
  !
  DO i = 1, nrow
    IF( dbcmask( i ) ) THEN
      count0 = count0 + 1;
      obj % dbcnptrs( count0 ) = i
      RowSize( i ) = count0
    END IF
  END DO
  !
  b = 0;
  DO i = 1, nrow
    DO j = obj % IA( i ), obj % IA( i + 1 ) - 1
      a = obj % JA( j )
      IF( dbcmask( a ) ) THEN
        b = b + 1
        call append( Intvec( RowSize( a ) ), [j, i] )
      END IF
    END DO
  END DO
  !
  call reallocate( obj % dbcJA, b,  obj % dbcIA, b, obj % dbcindx, tdbnptrs + 1 )
  a = 0; b = 0
  DO i = 1, tdbnptrs
    m = SIZE( intvec( i ) % Val )
    a = b + 1; b = b + m/2
    obj % dbcindx( i ) = a
    IF( m .eq. 0 ) cycle
    obj % dbcJA( a : b ) = intvec( i ) % Val( 1 : m : 2 )
    obj % dbcIA( a : b ) = intvec( i ) % Val( 2 : m : 2 )
  END DO
  obj % dbcindx( tdbnptrs + 1 ) = SIZE( obj % dbcJA ) + 1

  Deallocate( dbcmask, intvec, Rowsize, ColSize )
END PROCEDURE skit_setdbc_2

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_setmatrix
  To % A => From % A
END PROCEDURE skit_setmatrix

!----------------------------------------------------------------------------
!                                                                     Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_solve

  INTEGER( I4B ) :: i, j, a, b, n, fac
  REAL( DFP ) :: val
  !
  ! applying dbc
  !
  IF( ALLOCATED( obj % dbcnptrs ) ) THEN
    n = SIZE( obj % dbcnptrs )
    DO j = 1, n
      val = sol( obj % dbcnptrs( j ) )
      DO i = obj % dbcindx( j ), obj % dbcindx( j + 1 ) - 1
        rhs( obj % dbcIA( i ) ) = rhs( obj % dbcIA( i ) ) &
          & - obj % A( obj % dbcJA( i ) ) * val
        IF( obj % dbcnptrs( j ) .EQ. obj % dbcIA( i ) ) THEN
          obj % A( obj % dbcJA( i ) ) = 1.0_DFP
        ELSE
          obj % A( obj % dbcJA( i ) ) = 0.0_DFP
        END IF
      END DO
    END DO
    !
    DO i = 1, n
      rhs( obj % dbcnptrs( i ) ) = sol( obj % dbcnptrs( i ) )
    END DO
    !
    DO i = 1, n
      a = obj % IA( obj % dbcnptrs( i ) )
      b = obj % IA( obj % dbcnptrs( i ) + 1 ) - 1
      DO j = a, b
        IF( obj % JA( j ) .EQ. obj % dbcnptrs( i ) ) THEN
          obj % A( j ) = 1.0_DFP
        ELSE
          obj % A( j ) = 0.0_DFP
        END IF
      END DO
    END DO
  END IF
  !
  n = size( rhs )
  obj % ipar( 1 ) = 0
  a = 0 !its
  b = SIZE( obj % A ) !nnz
  fac = 3
  obj % ierr = 0
  ! make preconditioning
  IF( obj % ipar( 2 ) .NE. 0 ) THEN
100  IF( obj % ierr .EQ.  -2 .OR. obj % ierr .EQ. -3 ) THEN
        CALL reallocate( obj % ALU, fac * b, obj % JLU, fac * b )
      END IF
    SELECT CASE( obj % precondType )
    CASE( p_ilut )
      CALL ILUT( n, obj % A, obj % JA, obj % IA, &
        & obj % lfil, obj % droptol, &
        & obj % alu, obj % jlu, obj % ju, &
        & SIZE( obj % alu ), obj % w, obj % jw, obj % ierr )
    CASE( p_ilutp )
      CALL ILUTP( n, obj % A, obj % JA, obj % IA, &
        & obj % lfil, obj % droptol, obj % permtol, obj % mbloc, &
        & obj % alu, obj % jlu, obj % ju, &
        & SIZE( obj % alu ), obj % w, obj % jw, obj % iperm, obj % ierr )
    CASE( p_ilud )
      CALL ILUD( n, obj % A, obj % JA, obj % IA, &
        & obj % alpha, obj % droptol, obj % alu, obj % jlu, obj % ju, &
        & SIZE( obj % alu ), obj % w, obj % jw, obj % ierr )
    CASE( p_iludp )
      CALL ILUDP( n, obj % A, obj % JA, obj % IA, &
        & obj % alpha, obj % droptol, obj % permtol, obj % mbloc, &
        & obj % alu, obj % jlu, obj % ju, &
        & SIZE( obj % alu ), obj % w, obj % jw, obj % iperm, obj % ierr )
    END SELECT
    IF( obj % ierr .EQ.  -2 .OR. obj % ierr .EQ. -3 ) THEN
      fac = 2*fac
      goto 100
    END IF
  END IF

10   SELECT CASE( obj % SolverName )
  CASE( lis_cg )
    CALL CG( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_cgnr )
    CALL CGNR( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_bcg )
    CALL BCG( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_dbcg )
    CALL DBCG( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_bcgstab )
    CALL BCGSTAB( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_tfqmr )
    CALL TFQMR( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_fom )
    CALL FOM( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_gmres )
    CALL GMRES( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_fgmres )
    CALL FGMRES( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  CASE( lis_dqgmres )
    CALL DQGMRES( n, rhs, sol, obj % ipar, obj % fpar, obj % wk )
  END SELECT

  !<--- reading inside residue history
  IF( obj % ipar( 7 ) - a .GT. 0 ) THEN
    a = obj % ipar( 7 )
    obj % Res( a ) = obj % fpar( 6 )
  END IF

  obj % ierr = obj % ipar( 1 )

  SELECT CASE( obj % ipar( 1 ) )
  CASE( 1 )
    ! matvec with A
    CALL AMUX( n, obj % wk( obj % ipar( 8 ) ), &
      & obj % wk( obj % ipar( 9 ) ), obj % A, obj % ja, obj % ia )
    goto 10
  CASE( 2 )
    CALL ATMUX( n, obj % wk( obj % ipar( 8 ) ), &
      & obj % wk( obj % ipar( 9 ) ), obj % A, obj % ja, obj % ia )
    goto 10
  CASE( 3, 5 )
    CALL LUSOL( n, obj % wk( obj % ipar( 8 ) ), &
      & obj % wk( obj % ipar( 9 ) ), obj % alu, obj % jlu, obj % ju )
    goto 10
  CASE( 4, 6 )
    CALL LUTSOL( n, obj % wk( obj % ipar( 8 ) ), &
      & obj % wk( obj % ipar( 9 ) ), obj % alu, obj % jlu, obj % ju )
    goto 10
  CASE( 0 )
    obj % Res( 1 ) = obj % fpar( 3 )
    ! CALL Display( obj, '', stdout )
    ! CALL EQUALLINE( UnitNo = stdout )
    ! WRITE( stdout, "(A)" ) "ITERATIVE SOLVER HAS SATISFIED CONVERGENCE TEST"
    ! CALL EQUALLINE( UnitNo = stdout )
  CASE DEFAULT
    obj % Res( 1 ) = obj % fpar( 3 )
    IF ( obj % ipar(1) .eq. -1 ) THEN

      CALL Display( obj, '', stdout )
      CALL EQUALLINE( UnitNo = stdout )
      WRITE( stdout, "(A)" ) 'ERROR:: TOO MANY ITERATION'
      CALL EQUALLINE( UnitNo = stdout )

    ELSE IF ( obj % ipar(1) .eq. -2 ) THEN

      CALL Display( obj, '', stdout )
      CALL EQUALLINE( UnitNo = stdout )
      print *, 'ERROR :: NOT ENOUGH WORK SPACE'
      print *, '         The work space should at least have ', &
        & obj % ipar(4), ' elements.'
      CALL EQUALLINE( UnitNo = stdout )

    ELSE IF ( obj % ipar( 1 ) .eq. -3 ) THEN

      CALL Display( obj, '', stdout )
      CALL EQUALLINE( UnitNo = stdout )
      print *, 'ERROR :: BREAK-DOWN OF SOLVER'
      CALL EQUALLINE( UnitNo = stdout )

    ELSE
      CALL Display( obj, '', stdout )
      CALL EQUALLINE( UnitNo = stdout )
      print *, 'ERROR :: ITERATIVE SOLVER TERMINATED. CODE =', obj % ipar(1)
      CALL EQUALLINE( UnitNo = stdout )
    ENDIF

  END SELECT

END PROCEDURE skit_solve

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_display
  INTEGER( I4B ) :: I

  IF( PRESENT( unitno ) ) THEN
    I = unitno
  ELSE
    I = stdout
  END IF

  IF( LEN_TRIM( msg ) .NE. 0 ) WRITE( I, "(A)") TRIM( msg )

  CALL DASHLINE( UnitNo = I )
  WRITE( I, "(A)" ) "LIBRARY :: SPARSEKIT BY SAAD"
  CALL DASHLINE( UnitNo = I )
  SELECT CASE( obj % SolverName )
  CASE( lis_cg )
    WRITE( I, "(A)" ) "SOLVER NAME :: CG"
  CASE( lis_cgnr )
    WRITE( I, "(A)" ) "SOLVER NAME :: CGNR"
  CASE( lis_bcg )
    WRITE( I, "(A)" ) "SOLVER NAME :: BICG"
  CASE( lis_dbcg )
    WRITE( I, "(A)" ) "SOLVER NAME :: BICG WITH PIVOTING"
  CASE( lis_bcgstab )
    WRITE( I, "(A)" ) "SOLVER NAME :: BICG-STAB"
  CASE( lis_tfqmr )
    WRITE( I, "(A)" ) "SOLVER NAME :: TFQMR"
  CASE( lis_fom )
    WRITE( I, "(A)" ) "SOLVER NAME :: FOM"
  CASE( lis_gmres )
    WRITE( I, "(A)" ) "SOLVER NAME :: GMRES"
  CASE( lis_fgmres )
    WRITE( I, "(A)" ) "SOLVER NAME :: FGMRES"
  CASE( lis_dqgmres )
    WRITE( I, "(A)" ) "SOLVER NAME :: DQGMRES"
  END SELECT

  WRITE( I, "(A, I6)" ) "SIZE OF PROBLEM :: ", SIZE( obj % IA ) - 1

  SELECT CASE( obj % ipar( 2 ) )
  CASE( 0 )
    WRITE( I, "(A)" ) "PRECONDITIONING STATUS :: NO PRECONDITION"
  CASE( 1 )
    WRITE( I, "(A)" ) "PRECONDITIONING STATUS :: LEFT PRECONDITION"

  CASE( 2 )
    WRITE( I, "(A)" ) "PRECONDITIONING STATUS :: RIGHT PRECONDITION"
  END SELECT

  IF( obj % ipar( 2 ) .NE. 0 ) THEN
    WRITE( I, "(A, I4)" ) "PRECONDITIONING TYPE :: ", obj % precondType
    WRITE( I, "(A)" ) "----------------------------------------------"
    WRITE( I, "(A, I4)" )    "LFIL :: ", obj % lfil
    WRITE( I, "(A, I4)" )    "MBLOC :: ", obj % mbloc
    WRITE( I, "(A, G14.6)" ) "DROPTOL :: ", obj % droptol
    WRITE( I, "(A, G14.6)" ) "PERMTOL :: ", obj % permtol
    WRITE( I, "(A, G14.6)" ) "ALPHA :: ", obj % alpha
    WRITE( I, "(A)" ) "----------------------------------------------"
  END IF

  CALL Blanklines( nol = 1, unitno = I )

  SELECT CASE( obj % ipar( 3 ) )
  CASE( -2 )
    WRITE( I, "(A)" ) &
      & "CONVERG :: || dx(i) || <= rtol * || rhs || + atol"
  CASE( -1 )
    WRITE( I, "(A)" ) &
      & "CONVERG :: || dx(i) || <= rtol * || dx(1) || + atol"
  CASE( 0 )
    WRITE( I, "(A)" ) &
      & "CONVERG :: &
      & || residual || <= rtol * || initial residual || + atol"
  CASE( 1 )
    WRITE( I, "(A)" ) &
      & "CONVERG :: &
      & || residual || <= rtol * || initial residual || + atol"
  CASE( 2 )
    WRITE( I, "(A)" ) &
      & "CONVERG :: &
      & || residual || <= rtol * || rhs || + atol"
  END SELECT

  WRITE( I, "(A)" ) "----------------------------------------------"
  WRITE( I, "(A, G14.6)") "RELATIVE TOL :: ", obj % fpar( 1 )
  WRITE( I, "(A, G14.6)") "ABSOLUTE TOL :: ", obj % fpar( 2 )
  WRITE( I, "(A, I4)") "KRYLOV SIZE :: ", obj % ipar( 5 )
  WRITE( I, "(A, I4)") "MAX ITER :: ", obj % ipar( 6 )
  WRITE( I, "(A, I4)") "ITER :: ", obj % ipar( 7 )
  WRITE( I, "(A, I4)") "TOTAL INIT PERFORMED :: ", obj % ipar( 13 )
  WRITE( I, "(A, G14.6)") "INITIAL RES/ERROR0 :: ", obj % fpar( 3 )
  WRITE( I, "(A, G14.6)") "TARGET RES/ERROR :: ", obj % fpar( 4 )
  WRITE( I, "(A, G14.6)") "CURRENT RES/ERROR :: ", obj % fpar( 6 )
  WRITE( I, "(A, G14.6)") "CONV RATE :: ", obj % fpar( 7 )
  WRITE( I, "(A)" ) "----------------------------------------------"

  CALL Blanklines( nol = 1, unitno = I )
  IF( ASSOCIATED( obj % JA ) ) WRITE( I, "(A)" ) "JA :: ASSOCIATED"
  IF( ASSOCIATED( obj % IA ) ) WRITE( I, "(A)" ) "IA :: ASSOCIATED"
  IF( ALLOCATED( obj % JLU ) ) WRITE( I, "(A)" ) "JLU :: ALLOCATED"
  IF( ALLOCATED( obj % JU ) ) WRITE( I, "(A)" ) "JU :: ALLOCATED"
  IF( ALLOCATED( obj % IPERM ) ) WRITE( I, "(A)" ) "IPERM :: ALLOCATED"
  IF( ALLOCATED( obj % JW ) ) WRITE( I, "(A)" ) "JW :: ALLOCATED"
  IF( ASSOCIATED( obj % A ) ) WRITE( I, "(A)" ) "A :: ASSOCIATED"
  IF( ALLOCATED( obj % ALU ) ) WRITE( I, "(A)" ) "ALU :: ALLOCATED"
  IF( ALLOCATED( obj % WK ) ) WRITE( I, "(A)" ) "WK :: ALLOCATED"
  IF( ALLOCATED( obj % W ) ) WRITE( I, "(A)" ) "W :: ALLOCATED"

  CALL Blanklines( nol = 1, unitno = I )
  WRITE( I, "(A, I4)") "ERROR CODE :: ", obj % ierr
  CALL Blanklines( nol = 1, unitno = I )

END PROCEDURE skit_display

!----------------------------------------------------------------------------
!                                                         WriteResidueHistory
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_write_res_his
  TYPE( File_ ) :: aFile
  CHARACTER( LEN = LEN_TRIM( prefix ) + 15 ) :: filename
  INTEGER( I4B ) :: iter0, is, ie
  CHARACTER( LEN = 5 ) :: ext

  ie = INT( MINVAL( LOG10( obj % Res( 1 : obj % ipar( 7 ) ) ) ) ) - 1
  is = INT( MAXVAL( LOG10( obj % Res( 1 : obj % ipar( 7 ) ) ) ) ) + 1

  IF( PRESENT( iter ) ) THEN
    iter0 = iter
  ELSE
    iter0 = 0
  END IF

  IF( fmt .EQ. 'BIN' ) THEN
    ext = '.bin'
  ELSE
    ext = '.txt'
  END IF

  filename = TRIM( prefix ) // '_res_' // TRIM( INT2STR( iter0 ) )

  CALL OpenFileToWrite( obj = aFile, Path = Path, FileName = TRIM( filename ), &
    & Extension = TRIM( ext ) )

  DO iter0 = 1, obj % ipar( 7 )
    WRITE( aFile % UnitNo, '(I6, 4X, G16.6)' ) iter0, obj % Res( iter0 )
  END DO

  CALL CloseFile( aFile )

  CALL OpenFileToWrite( aFile, Path, TRIM( filename ), &
    & Extension = '.gp' )
  WRITE( aFile % UnitNo, '(A)' ) '# Gnuplot script file'
  WRITE( aFile % UnitNo, '(A)' ) '# Author :: Vikas Sharma'
  WRITE( aFile % UnitNo, '(A)' ) &
    & '# Generated from Sparsekit_Class.F90>>WriteResidueHistory()'
  WRITE( aFile % UnitNo, '(A)' ) &
    & "set terminal postscript eps enhance color font 'Helvetica,10'"
  WRITE( aFile % UnitNo, '(A)' ) &
    & "set output '"//trim( path ) &
      & // trim( filename ) // ".eps'"
  WRITE( aFile % UnitNo, '(A)' ) &
    & "set xlabel 'Iteration No'"
  WRITE( aFile % UnitNo, '(A)' ) "set ylabel '||residue||'"

  WRITE( aFile % UnitNo, '(A)' ) "set logscale y"

  WRITE( aFile % UnitNo, '(A)' ) "set size ratio -1"

  WRITE( aFile % UnitNo, '(A)' ) &
    & "set title 'its = "//TRIM( INT2STR( iter0 ) )// "'"

  WRITE( aFile % UnitNo, '(A)' ) &
    & 'set xrange[1:'//TRIM( INT2STR( obj % ipar( 7 ) + 5 ) )//"]"

  WRITE( aFile % UnitNo, '(A)' ) &
    & 'set yrange[' // '1.0E'// TRIM( INT2STR( ie ) ) // " : 1.0E" // &
    & TRIM( INT2STR( is ) ) // "]"

  WRITE( aFile % UnitNo, "(A)" ) &
  & "plot" // "'"// TRIM( path ) // TRIM( filename ) &
  & // trim( ext ) &
  & // "' with linespoints pointtype 7 pointsize 1.0 linetype 1 linewidth 0.5"

CALL CloseFile( aFile )

END PROCEDURE skit_write_res_his
!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE skit_Deallocate
  IF( ALLOCATED( obj % dbcnptrs ) ) DEALLOCATE( obj % dbcnptrs )
  IF( ALLOCATED( obj % dbcIndx ) ) DEALLOCATE( obj % dbcIndx )
  IF( ALLOCATED( obj % dbcJA ) ) DEALLOCATE( obj % dbcJA )
  IF( ALLOCATED( obj % dbcIA ) ) DEALLOCATE( obj % dbcIA )
  obj % IA => NULL( )
  obj % JA => NULL( )
  IF( ALLOCATED( obj % JLU ) ) DEALLOCATE( obj % JLU )
  IF( ALLOCATED( obj % JU ) ) DEALLOCATE( obj % JU )
  IF( ALLOCATED( obj % IPERM ) ) DEALLOCATE( obj % IPERM )
  IF( ALLOCATED( obj % JW ) ) DEALLOCATE( obj % JW )
  obj % A => NULL( )
  IF( ALLOCATED( obj % ALU ) ) DEALLOCATE( obj % ALU )
  IF( ALLOCATED( obj % WK ) ) DEALLOCATE( obj % WK )
  IF( ALLOCATED( obj % W ) ) DEALLOCATE( obj % W )
  obj % ipar = 0
  obj % fpar = 0
  obj % ierr = 0
END PROCEDURE skit_Deallocate

END SUBMODULE Sparsekit

!! set Dirichlet boundary condition information

!> authors: Dr. Vikas Sharma
!
! This subroutine set the Dirichlet boundary condition in the linear solver
! In this case all DOFs have the same dirichlet nodes pointers
! `Nptrs` denotes the dirichlet node numbers
! `storageFMT` can be `DOF_FMT` or `Nodes_FMT`

SUBROUTINE ls_setDBC_1( obj, Nptrs, dofs )
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: dofs( : )

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
END SUBROUTINE ls_setDBC_1


!! set Dirichlet boundary condition information

MODULE SUBROUTINE ls_setDBC_2( obj, Nptrs, dofs )
  CLASS( LinSolver_ ), INTENT( INOUT) :: obj
  TYPE( IntVector_ ), INTENT( IN ) :: Nptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: dofs( : )

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
END SUBROUTINE ls_setDBC_2

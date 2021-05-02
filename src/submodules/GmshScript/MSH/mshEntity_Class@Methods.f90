SUBMODULE( mshEntity_Class ) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               gotoEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_goto
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, Reopen
  CHARACTER( LEN = 100 ) :: Dummy
  !
  ! Find $MeshFormat
  Reopen = 0
  ierr = .FALSE.
  DO
    READ( mshFile % UnitNo, "(A)", IOSTAT = IOSTAT ) Dummy
    IF( IOSTAT .LT. 0 ) THEN
      CALL ReOpenFile( mshFile )
      Reopen = Reopen + 1
    ELSE IF( IOSTAT .GT. 0 .OR. Reopen .GT. 1 ) THEN
      ierr = .TRUE.
      EXIT
    ELSE IF( TRIM( Dummy ) .EQ. '$Entities' ) THEN
      EXIT
    END IF
  END DO
END PROCEDURE ent_goto

!----------------------------------------------------------------------------
!                                                           ReadPointEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_read_point
  ! Define internal variables
  LOGICAL( LGT ) :: dummyierr
  INTEGER( I4B ) :: Intvec( 100 ), n, i
  ! go to tag
  IF( ReadTag ) THEN
    CALL obj % GotoTag( mshFile, ierr )
    dummyierr = ierr
  ELSE
    dummyierr = .FALSE.
  END IF
  !
  IF( .NOT. dummyierr ) THEN
    obj % XiDim = 0
    READ( mshFile % UnitNo, * ) obj % Uid, obj % X, obj % Y, obj % Z, &
      & n, (Intvec(i), i=1,n)

    IF( ALLOCATED( obj % PhysicalTag ) ) DEALLOCATE( obj % PhysicalTag )

    IF( n .NE. 0 ) THEN
      ALLOCATE( obj % PhysicalTag( n ) )
      obj % PhysicalTag( 1 : n ) = Intvec( 1 : n )
    END IF

  END IF

END PROCEDURE ent_read_point

!----------------------------------------------------------------------------
!                                                           ReadCurveEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_read_Curve

  LOGICAL( LGT ) :: dummyierr
  INTEGER( I4B ) :: Intvec1( 100 ), n, i, m, Intvec2( 100 )

  IF( ReadTag ) THEN
    CALL obj % GotoTag( mshFile, ierr )
    dummyierr = ierr
  ELSE
    dummyierr = .FALSE.
  END IF
  IF( .NOT. dummyierr ) THEN
    obj % XiDim = 1
    READ( mshFile % UnitNo, * ) obj % Uid, obj % minX, obj % minY, obj % minZ, &
      & obj % maxX, obj % maxY, obj % maxZ, &
      & n, (Intvec1(i), i=1,n), &
      & m, (Intvec2(i), i=1,m)
    !
    IF( ALLOCATED( obj % PhysicalTag ) ) DEALLOCATE( obj % PhysicalTag )
    IF( ALLOCATED( obj % BoundingEntity ) ) DEALLOCATE( obj % BoundingEntity )
    !
    IF( n .NE. 0 ) THEN
      ALLOCATE( obj % PhysicalTag( n ) )
      obj % PhysicalTag( 1 : n ) = Intvec1( 1 : n )
    END IF
    IF( m .NE. 0 ) THEN
      ALLOCATE( obj % BoundingEntity( m ) )
      obj % BoundingEntity( 1 : m ) = Intvec2( 1 : m )
    END IF
  END IF
END PROCEDURE ent_read_Curve

!----------------------------------------------------------------------------
!                                                           ReadSurfaceEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_read_Surface
  ! Define internal variables
  LOGICAL( LGT ) :: dummyierr
  INTEGER( I4B ), ALLOCATABLE :: Intvec1( : ), Intvec2( : )
  INTEGER( I4B ) :: n, i, m
  TYPE( String ) :: aline
  TYPE( String ), ALLOCATABLE :: entries( : )
  !
  IF( ReadTag ) THEN
    CALL obj % GotoTag( mshFile, ierr )
    dummyierr = ierr
  ELSE
    dummyierr = .FALSE.
  END IF

  IF( .NOT. dummyierr ) THEN
    obj % XiDim = 2
    ! READ( mshFile % UnitNo, * ) obj % Uid, obj % minX, obj % minY, &
    !   & obj % minZ,obj % maxX, obj % maxY, obj % maxZ, &
    !   & n, (Intvec1(i), i=1,n), &
    !   & m, (Intvec2(i), i=1,m)
    call aline % read_line( unit = mshFile % Unitno )
    call aline%split(tokens=entries, sep=' ')
    obj % Uid = entries( 1 ) % to_number( kind = I4B )

    obj % minX = entries( 2 ) % to_number( kind = DFP )
    obj % minY = entries( 3 ) % to_number( kind = DFP )
    obj % minZ = entries( 4 ) % to_number( kind = DFP )

    obj % maxX = entries( 5 ) % to_number( kind = DFP )
    obj % maxY = entries( 6 ) % to_number( kind = DFP )
    obj % maxZ = entries( 7 ) % to_number( kind = DFP )

    n = entries( 8 ) % to_number( kind = I4B )
    IF( n .NE. 0 ) THEN
      ALLOCATE( IntVec1( n ) )
      DO i = 1, n
        IntVec1( i ) = entries( 8 + i ) % to_number( kind = I4B )
      END DO
    ENDIF
    !! check total length here
    m = entries( 9 + n ) % to_number( kind = I4B )
    IF( m .NE. 0 ) THEN
      ALLOCATE( IntVec2( m ) )
      DO i = 1, m
        IntVec2( i ) = entries( 9 + n + i ) % to_number( kind = I4B )
      END DO
    ENDIF

    IF( ALLOCATED( obj % PhysicalTag ) ) DEALLOCATE( obj % PhysicalTag )
    IF( ALLOCATED( obj % BoundingEntity ) ) DEALLOCATE( obj % BoundingEntity )

    IF( n .NE. 0 ) THEN
      ALLOCATE( obj % PhysicalTag( n ) )
      obj % PhysicalTag( 1 : n ) = Intvec1( 1 : n )
    END IF

    IF( m .NE. 0 ) THEN
      ALLOCATE( obj % BoundingEntity( m ) )
      obj % BoundingEntity( 1 : m ) = Intvec2( 1 : m )
    END IF
  END IF

  IF( ALLOCATED( IntVec1 ) ) DEALLOCATE( IntVec1 )
  IF( ALLOCATED( IntVec2 ) ) DEALLOCATE( IntVec2 )
  IF( ALLOCATED( entries ) ) DEALLOCATe( Entries )
END PROCEDURE ent_read_Surface

!----------------------------------------------------------------------------
!                                                           ReadVolumeEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_read_Volume
  ! Define internal variables
  LOGICAL( LGT ) :: dummyierr
  INTEGER( I4B ), ALLOCATABLE :: Intvec1( : ), Intvec2( : )
  INTEGER( I4B ) :: n, i, m
  TYPE( String ) :: aline
  TYPE( String ), ALLOCATABLE :: entries( : )
  !
  IF( ReadTag ) THEN
    CALL obj % GotoTag( mshFile, ierr )
    dummyierr = ierr
  ELSE
    dummyierr = .FALSE.
  END IF
  !
  IF( .NOT. dummyierr ) THEN
    obj % XiDim = 3
    ! READ( mshFile % UnitNo, * ) obj % Uid, obj % minX, obj % minY, &
    !   & obj % minZ, obj % maxX, obj % maxY, obj % maxZ, &
    !   & n, (Intvec1(i), i=1,n), &
    !   & m, (Intvec2(i), i=1,m)

    call aline % read_line( unit = mshFile % Unitno )
    call aline%split(tokens=entries, sep=' ')
    obj % Uid = entries( 1 ) % to_number( kind = I4B )

    obj % minX = entries( 2 ) % to_number( kind = DFP )
    obj % minY = entries( 3 ) % to_number( kind = DFP )
    obj % minZ = entries( 4 ) % to_number( kind = DFP )

    obj % maxX = entries( 5 ) % to_number( kind = DFP )
    obj % maxY = entries( 6 ) % to_number( kind = DFP )
    obj % maxZ = entries( 7 ) % to_number( kind = DFP )

    n = entries( 8 ) % to_number( kind = I4B )
    IF( n .NE. 0 ) THEN
      ALLOCATE( IntVec1( n ) )
      DO i = 1, n
        IntVec1( i ) = entries( 8 + i ) % to_number( kind = I4B )
      END DO
    ENDIF
    !! check total length here
    m = entries( 9 + n ) % to_number( kind = I4B )
    IF( m .NE. 0 ) THEN
      ALLOCATE( IntVec2( m ) )
      DO i = 1, m
        IntVec2( i ) = entries( 9 + n + i ) % to_number( kind = I4B )
      END DO
    ENDIF

    IF( ALLOCATED( obj % PhysicalTag ) ) DEALLOCATE( obj % PhysicalTag )
    IF( ALLOCATED( obj % BoundingEntity ) ) DEALLOCATE( obj % BoundingEntity )
    !
    IF( n .NE. 0 ) THEN
      ALLOCATE( obj % PhysicalTag( n ) )
      obj % PhysicalTag( 1 : n ) = Intvec1( 1 : n )
    END IF
    !
    IF( m .NE. 0 ) THEN
      ALLOCATE( obj % BoundingEntity( m ) )
      obj % BoundingEntity( 1 : m ) = Intvec2( 1 : m )
    END IF
  END IF

  IF( ALLOCATED( IntVec1 ) ) DEALLOCATE( IntVec1 )
  IF( ALLOCATED( IntVec2 ) ) DEALLOCATE( IntVec2 )
  IF( ALLOCATED( entries ) ) DEALLOCATe( Entries )

END PROCEDURE ent_read_Volume

!----------------------------------------------------------------------------
!                                                           WriteToFile
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_write_file

END PROCEDURE ent_write_file

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_display
  !Define intent of dummy variables
  INTEGER( I4B ) :: I, j
  TYPE( String ) :: Str1

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  !
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  !
  CALL BlankLines( UnitNo = I, NOL = 1 )
  WRITE( I, "(A)" ) "| Property | Value |"
  WRITE( I, "(A)" ) "| :---     | ---:  |"
  WRITE( I, "(A, I4, A )" ) " | Tag | ", obj % UiD, " | "
  SELECT CASE( obj % XiDim )
  CASE( 0 )
    WRITE( I, "(A)" ) " | Type |  Point | "
    WRITE( I, "(A, 3(G13.6, ','), A)" ) " | X, Y, Z | ", obj % X, &
      & obj % Y, obj % Z, " | "
  CASE( 1 )
    WRITE( I, "(A)" ) " | Type |  Curve | "
    WRITE( I, "(A, 3(G13.6, ','), A)" ) " | minX, minY, minZ | ", &
      & obj % minX, obj % minY, obj % minZ, " | "
    WRITE( I, "(A, 3(G13.6, ','), A)" ) " | maxX, maxY, maxZ | ", &
      & obj % maxX, obj % maxY, obj % maxZ, " | "
    Str1 = String( Str( SIZE( obj % BoundingEntity ), .true. ) )
    WRITE( I, "(A, "//TRIM( Str1 )//"(I4, ','), A)" ) &
      & "| Bounding Points |", obj % BoundingEntity, " |"
  CASE( 2 )
    WRITE( I, "(A)" ) " | Type |  Surface | "
    WRITE( I, "(A, 3(G13.6, ','), A)" ) " | minX, minY, minZ | ", &
      & obj % minX, obj % minY, obj % minZ, " | "
    WRITE( I, "(A, 3(G13.6, ','), A)" ) " | maxX, maxY, maxZ | ", &
      & obj % maxX, obj % maxY, obj % maxZ, " | "
    Str1 = String( Str( SIZE( obj % BoundingEntity ), .true. ) )
    WRITE( I, "(A, "//TRIM( Str1 )//"(I4, ','), A)" ) &
      & "| Bounding Curves |", obj % BoundingEntity, " |"
  CASE( 3 )
    WRITE( I, "(A)" ) " | Type |  Volume | "
    WRITE( I, "(A, 3(G13.6, ','), A)" ) " | minX, minY, minZ | ", &
      & obj % minX, obj % minY, obj % minZ, " | "
    WRITE( I, "(A, 3(G13.6, ','), A)" ) " | maxX, maxY, maxZ | ", &
      & obj % maxX, obj % maxY, obj % maxZ, " | "
    Str1 = String( Str( SIZE( obj % BoundingEntity ), .true. ) )
    WRITE( I, "(A, "//TRIM( Str1 )//"(I4, ','), A)" ) &
      & "| Bounding Surfaces |", obj % BoundingEntity, " |"
  END SELECT
  ! Physical Tag
  IF( ALLOCATED( obj % PhysicalTag ) ) THEN
    Str1 = String( Str( SIZE( obj % PhysicalTag ), .true. ) )
    WRITE( I, "(A, "//TRIM( Str1 )//"(I4, ','), A)" ) &
      & "| Physical Tag |", obj % PhysicalTag, " | "
  END IF
  ! Nodes
  IF( ALLOCATED( obj % NodeNumber ) ) THEN
    WRITE( I, "(A, I4)" ) "| Total Nodes |", SIZE( obj % NodeNumber )
    WRITE( I, "(A)" ) "| Node Number | Coordinates |"
    DO j = 1, SIZE( obj % NodeNumber )
      WRITE( I, "(A, I4, A, 3(G13.6, ','), A)" ) &
      & "| ", obj % NodeNumber( j ), " | ", obj % NodeCoord( 1:3, j), " |"
    END DO
  END IF
  ! Elements
  IF( ALLOCATED( obj % ElemNumber ) ) THEN
    WRITE( I, "(A, I4)" ) "| Total Elements |", SIZE( obj % ElemNumber )
    WRITE( I, "(A)" ) "| Element Number | Connectivity |"
    Str1 = String( Str( SIZE( obj % Nptrs, 1 ), .true. ) )
    DO j = 1, SIZE( obj % ElemNumber )
      WRITE( I, "(A, I4, A, "//TRIM(Str1)//"(G13.6, ','), A)" ) &
      & "| ", obj % ElemNumber( j ), " | ", obj % Nptrs( 1:, j), " |"
    END DO
  END IF
END PROCEDURE ent_display

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_getIndex_a
  ! Define internal variables
  INTEGER( I4B ) :: j, tSize
  ans = 0
  tSize = SIZE( mshEntities )
  DO j = 1, tSize
    IF( mshEntities( j ) % UiD .EQ. UiD ) THEN
      ans = j
      EXIT
    END IF
  END DO
END PROCEDURE ent_getIndex_a

!----------------------------------------------------------------------------
!                                                          TotalPhysicalTags
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_tphysicaltag
  IF( ALLOCATED( obj % PhysicalTag ) ) THEN
    ans = SIZE( obj % PhysicalTag )
  ELSE
    ans = 0
  END IF
END PROCEDURE ent_tphysicaltag

!----------------------------------------------------------------------------
!                                                          TotalBoundingTags
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_tBoundingtag
  IF( ALLOCATED( obj % BoundingEntity ) ) THEN
    ans = SIZE( obj % BoundingEntity )
  ELSE
    ans = 0
  END IF
END PROCEDURE ent_tBoundingtag

!----------------------------------------------------------------------------
!                                                              TotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_tElements
  IF( ALLOCATED( obj % ElemNumber ) ) THEN
    ans = SIZE( obj % ElemNumber )
  ELSE
    ans = 0
  END IF
END PROCEDURE ent_tElements

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE ent_deallocatedata
  obj % uid = 0
  obj % XiDim = 0
  obj % ElemType = 0
  obj % minX = 0.0
  obj % minY = 0.0
  obj % minZ = 0.0
  obj % maxX = 0.0
  obj % maxY = 0.0
  obj % maxZ = 0.0
  obj % X = 0.0
  obj % Y = 0.0
  obj % Z = 0.0

  IF( ALLOCATED( obj % PhysicalTag ) ) DEALLOCATE( obj % PhysicalTag )
  IF( ALLOCATED( obj % NodeNumber ) ) DEALLOCATE( obj % NodeNumber )
  IF( ALLOCATED( obj % ElemNumber ) ) DEALLOCATE( obj % ElemNumber )
  IF( ALLOCATED( obj % Nptrs ) ) DEALLOCATE( obj % Nptrs )
  IF( ALLOCATED( obj % BoundingEntity ) ) DEALLOCATE( obj % BoundingEntity )
  IF( ALLOCATED( obj % NodeCoord ) ) DEALLOCATE( obj % NodeCoord )


END PROCEDURE ent_deallocatedata
END SUBMODULE Methods
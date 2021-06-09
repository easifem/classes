SUBMODULE( mshPhysicalNames_Class ) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                          GotoTag
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_goto
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, Reopen
  CHARACTER( LEN = 100 ) :: Dummy
  ! Find $PhysicalNames
  Reopen = 0
  ierr = .FALSE.
  DO
    READ( mshFile % UnitNo, "(A)", IOSTAT = IOSTAT ) Dummy
    IF( IOSTAT .LT. 0 ) THEN
      CALL ReopenFile( mshFile )
      Reopen = Reopen + 1
    ELSE IF( IOSTAT .GT. 0 .OR. Reopen .GT. 1 ) THEN
      ierr = .TRUE.
      EXIT
    ELSE IF( TRIM( Dummy ) .EQ. '$PhysicalNames' ) THEN
      EXIT
    END IF
  END DO
END PROCEDURE pn_goto

!----------------------------------------------------------------------------
!                                                               ReadFromFile
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_read_file
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, tp, k
  CHARACTER( LEN = 120 ) :: dummystr
  ! Go to $PhysicalNames
  CALL obj % GotoTag( mshFile, ierr )
  !
  IF( .NOT. ierr ) THEN
    READ( mshFile % UnitNo, * ) tp
    IF( ALLOCATED( obj % NSD ) ) DEALLOCATE( obj % NSD )
    IF( ALLOCATED( obj % Tag ) ) DEALLOCATE( obj % Tag )
    IF( ALLOCATED( obj % PhysicalName ) ) DEALLOCATE( obj % PhysicalName )
    IF( ALLOCATED( obj % numElements ) ) DEALLOCATE( obj % numElements )
    IF( ALLOCATED( obj % numNodes ) ) DEALLOCATE( obj % numNodes )
    ALLOCATE( &
      & obj % numElements( tp ), &
      & obj % numNodes( tp ), &
      & obj % NSD( tp ), &
      & obj % Tag( tp ), &
      & obj % PhysicalName( tp ), &
      & obj % Entities( tp ) &
      & )
    obj % numElements = 0; obj % numNodes = 0 ! init
    DO k = 1, tp
      READ( mshFile % UnitNo, *, IOSTAT = IOSTAT ) obj % NSD( k ), &
        & obj % Tag( k ), dummystr
      obj % PhysicalName( k ) = String( dummystr )
      dummystr = ""
    END DO
  END IF
END PROCEDURE pn_read_file

!----------------------------------------------------------------------------
!                                                               WriteToFile
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_write_file
  ! Define internal variables
  INTEGER( I4B ) :: k, tp
  TYPE( File_ ) :: outFile
  !
  CALL OpenFileToWrite( &
    & outFile, &
    & mshFile % Path % Raw, &
    & TRIM( mshFile % FileName % Raw ) //"_PhysicalNames", &
    & mshFile % Extension % Raw )
  tp = SIZE( obj % NSD )
  !
  IF( PRESENT( Str ) ) THEN
    WRITE( outFile % UnitNo, "(A)" ) TRIM( Str )
  END IF
  !
  WRITE( outFile % UnitNo, "(I6)") SIZE( obj % NSD )
  !
  DO k = 1, tp
    WRITE( outFile % UnitNo, * ) obj % NSD( k ), obj % Tag( k ), &
      & '"'//TRIM( obj % PhysicalName( k ) % Raw ) // '"'
  END DO
  !
  IF( PRESENT( EndStr )) THEN
    WRITE( outFile % UnitNo, "(A)" ) TRIM( EndStr )
  END IF
  CALL CloseFile( outFile )
END PROCEDURE pn_write_file

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_display
  ! Define internal variables
  INTEGER( I4B ) :: I, tSize, j
  TYPE( String ) :: Str1, Str2

  ! set unit number
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  ! print message
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  ! get total size info
  tSize = obj % SIZE( )
  IF( tSize .NE. 0 ) THEN
    ! write header in markdown
    CALL BlankLines( UnitNo = I, NOL = 1 )
    WRITE( I, "(A)" ) "| Sr.No. |  NSD  | Physical Tag | Physical Name | &
      & NumElem | NumNodes |"
    WRITE( I, "(A)" ) "| :---   | :---: | :---:        | :---:          | &
      & :---: | ---: |"
    ! Write entries one by one
    DO j = 1, tSize
      Str2 = Str1 % Join( [ &
        & String( "| "// TRIM( Str( j, .true. ) ) ), &
        & String( Str( obj % NSD( j ), .true. ) ), &
        & String( Str( obj % Tag( j ), .true. ) ), &
        & TRIM( obj % PhysicalName( j ) ), &
        & String( Str( obj % numElements( j ), .true. ) ), &
        & String( Str( obj % numNodes( j ), .true. ) ) ], " | " )
      WRITE( I, "(A)") TRIM( Str2 ) // " | "
    END DO
    !
    IF( ALLOCATED( obj % Entities ) ) THEN
      CALL BlankLines( UnitNo = I, NOL = 1 )
      WRITE( I, "(A)" ) "Physical Tag to Entities Tag"
      WRITE( I, "(A)") "| Physical Tag | PhysicalName | Entities Tag |"
      WRITE( I, "(A)") "| :--- | :---: | ---: |"
      DO j = 1, SIZE( obj % Entities )
        tSize = SIZE( obj % Entities( j ) )
        WRITE( I, "( A, I4, A, " // TRIM( Str( tSize, .false. ) ) &
          & // "(I4,',')"//", A )" ) &
          & "| ", obj % Tag( j ), &
          & "| "//TRIM( obj % PhysicalName( j ) )//" | ", &
          & obj % Entities( j ) % Val, " |"
      END DO
    END IF
  END IF
END PROCEDURE pn_display

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_get_size
  IF( ALLOCATED( obj % NSD ) ) THEN
    ans = SIZE( obj % NSD )
  ELSE
    ans = 0
  END IF
END PROCEDURE pn_get_size

!----------------------------------------------------------------------------
!                                                        TotalPhysicalPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_size_point
  IF( ALLOCATED( obj % NSD ) ) THEN
    ans = COUNT( obj % NSD .EQ. 0 )
  ELSE
    ans = 0
  END IF
END PROCEDURE pn_size_point

!----------------------------------------------------------------------------
!                                                        TotalPhysicalCurves
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_size_Curve
  IF( ALLOCATED( obj % NSD ) ) THEN
    ans = COUNT( obj % NSD .EQ. 1 )
  ELSE
    ans = 0
  END IF
END PROCEDURE pn_size_Curve

!----------------------------------------------------------------------------
!                                                      TotalPhysicalSurfaces
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_size_Surface
  IF( ALLOCATED( obj % NSD ) ) THEN
    ans = COUNT( obj % NSD .EQ. 2 )
  ELSE
    ans = 0
  END IF
END PROCEDURE pn_size_Surface

!----------------------------------------------------------------------------
!                                                       TotalPhysicalVolumes
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_size_Volume
  IF( ALLOCATED( obj % NSD ) ) THEN
    ans = COUNT( obj % NSD .EQ. 3 )
  ELSE
    ans = 0
  END IF
END PROCEDURE pn_size_Volume

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_a
  ! Define internal variables
  INTEGER( I4B ) :: j, tSize

  ans = 0
  IF( ALLOCATED( obj % NSD ) ) THEN
    tSize = SIZE( obj % NSD )
    DO j = 1, tSize
      IF( obj % PhysicalName( j ) .EQ. TRIM( Name ) ) THEN
        ans = j
        EXIT
      END IF
    END DO
  END IF

END PROCEDURE pn_index_a

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_b
  ! Define internal variables
  INTEGER( I4B ) :: j, m, i, n
  !
  m = SIZE( Name ); ans = 0
  !
  IF( ALLOCATED( obj % NSD ) ) THEN
    n = SIZE( obj % NSD )
    DO i = 1, m
      DO j = 1, n
        IF( obj % PhysicalName( j ) .EQ. Name( i ) ) THEN
          ans( i ) = j
          EXIT
        END IF
      END DO
    END DO
  END IF
END PROCEDURE pn_index_b

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_c
  ! Define internal variables
  INTEGER( I4B ) :: tPoints, k, XiDim, Tag

  XiDim = XiDimTag( 1 ); Tag = XiDimTag( 2 )
  tPoints = SIZE( obj % NSD )
  DO k = 1, tPoints
    IF( obj % NSD( k ) .EQ. XiDim .AND. obj % Tag( k ) .EQ. Tag ) THEN
      ans = k
    END IF
  END DO
END PROCEDURE pn_index_c

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_d
  ! Define internal variables
  INTEGER( I4B ) :: tPoints, k, i

  SELECT CASE( XiDimTag )
  CASE( 0 )
    tPoints = obj % TotalPhysicalPoints( )
  CASE( 1 )
    tPoints = obj % TotalPhysicalCurves( )
  CASE( 2 )
    tPoints = obj % TotalPhysicalSurfaces( )
  CASE( 3 )
    tPoints = obj % TotalPhysicalVolumes( )
  END SELECT

  ALLOCATE( ans( tPoints ) )
  tPoints = SIZE( obj % NSD ); i = 0
  DO k = 1, tPoints
    IF( obj % NSD( k ) .EQ. XiDimTag ) THEN
      i = i + 1
      ans( i ) = k
    END IF
  END DO

END PROCEDURE pn_index_d

!----------------------------------------------------------------------------
!                                                          PhysicalPointNames
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Point_names
  ! Define internal variables
  INTEGER( I4B ) :: tPoints, i, k

  tPoints = obj % TotalPhysicalPoints( )
  ALLOCATE( ans( tPoints ) )

  tPoints = SIZE( obj % NSD ); i = 0;
  DO k = 1, tPoints
    IF( obj % NSD( k ) .EQ. 0 ) THEN
      i = i + 1
      ans( i ) = obj % PhysicalName( k ) % Chars( )
    END IF
  END DO
END PROCEDURE pn_Point_names

!----------------------------------------------------------------------------
!                                                          PhysicalCurveNames
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Curve_names
  ! Define internal variables
  INTEGER( I4B ) :: tLines, i, k

  tLines = obj % TotalPhysicalCurves( )
  ALLOCATE( ans( tLines ) )

  tLines = SIZE( obj % NSD ); i = 0;
  DO k = 1, tLines
    IF( obj % NSD( k ) .EQ. 1 ) THEN
      i = i + 1
      ans( i ) = obj % PhysicalName( k ) % Chars( )
    END IF
  END DO
END PROCEDURE pn_Curve_names

!----------------------------------------------------------------------------
!                                                       PhysicalSurfaceNames
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Surface_names
  ! Define internal variables
  INTEGER( I4B ) :: tSurfaces, i, k

  tSurfaces = obj % TotalPhysicalSurfaces( )
  ALLOCATE( ans( tSurfaces ) )

  tSurfaces = SIZE( obj % NSD ); i = 0;
  DO k = 1, tSurfaces
    IF( obj % NSD( k ) .EQ. 2 ) THEN
      i = i + 1
      ans( i ) = obj % PhysicalName( k ) % Chars( )
    END IF
  END DO

END PROCEDURE pn_Surface_names

!----------------------------------------------------------------------------
!                                                       PhysicalVolumeNames
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Volume_names
  ! Define internal variables
  INTEGER( I4B ) :: tVolumes, i, k
  !
  tVolumes = obj % TotalPhysicalVolumes( )
  ALLOCATE( ans( tVolumes ) )
  !
  tVolumes = SIZE( obj % NSD ); i = 0;
  DO k = 1, tVolumes
    IF( obj % NSD( k ) .EQ. 3 ) THEN
      i = i + 1
      ans( i ) = obj % PhysicalName( k ) % Chars( )
    END IF
  END DO
END PROCEDURE pn_Volume_names

!----------------------------------------------------------------------------
!                                                          PhysicalPointTags
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Point_tags
  ! Define internal variables
  INTEGER( I4B ) :: tPoints, i, k

  tPoints = obj % TotalPhysicalPoints( )
  ALLOCATE( ans( tPoints ) )

  tPoints = SIZE( obj % NSD ); i = 0;
  DO k = 1, tPoints
    IF( obj % NSD( k ) .EQ. 0 ) THEN
      i = i + 1
      ans( i ) = obj % Tag( k )
    END IF
  END DO
END PROCEDURE pn_Point_tags

!----------------------------------------------------------------------------
!                                                          PhysicalCurveTag
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Curve_tags
  ! Define internal variables
  INTEGER( I4B ) :: tLines, i, k
  !
  tLines = obj % TotalPhysicalCurves( )
  ALLOCATE( ans( tLines ) )
  !
  tLines = SIZE( obj % NSD ); i = 0;
  DO k = 1, tLines
    IF( obj % NSD( k ) .EQ. 1 ) THEN
      i = i + 1
      ans( i ) = obj % Tag( k )
    END IF
  END DO
END PROCEDURE pn_Curve_tags

!----------------------------------------------------------------------------
!                                                         PhysicalSurfaceTag
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Surface_tags
  ! Define internal variables
  INTEGER( I4B ) :: tSurfaces, i, k

  tSurfaces = obj % TotalPhysicalSurfaces( )
  ALLOCATE( ans( tSurfaces ) )

  tSurfaces = SIZE( obj % NSD ); i = 0;
  DO k = 1, tSurfaces
    IF( obj % NSD( k ) .EQ. 2 ) THEN
      i = i + 1
      ans( i ) = obj % Tag( k )
    END IF
  END DO
END PROCEDURE pn_Surface_tags

!----------------------------------------------------------------------------
!                                                          PhysicalVolumeTag
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Volume_tags
  ! Define internal variables
  INTEGER( I4B ) :: tVolumes, i, k

  tVolumes = obj % TotalPhysicalVolumes( )
  ALLOCATE( ans( tVolumes ) )

  tVolumes = SIZE( obj % NSD ); i = 0;
  DO k = 1, tVolumes
    IF( obj % NSD( k ) .EQ. 3 ) THEN
      i = i + 1
      ans( i ) = obj % Tag( k )
    END IF
  END DO
END PROCEDURE pn_Volume_tags

!----------------------------------------------------------------------------
!                                                                 WhoAmI
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_who_am_i
  ! Define internal variables
  INTEGER( I4B ) :: NSD

  NSD = obj % NSD( I )

  SELECT CASE( NSD )
  CASE( 0 )
    ans = "PhysicalPoint"
  CASE( 1 )
    ans = "PhysicalLine"
  CASE( 2 )
    ans = "PhysicalSurface"
  CASE( 3 )
    ans = "PhysicalVolume"
  END SELECT
END PROCEDURE pn_who_am_i

!----------------------------------------------------------------------------
!                                                      IndexOfPhysicalPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_point
  ans = obj % getIndex( [0_I4B, Tag] )
END PROCEDURE pn_index_point

!----------------------------------------------------------------------------
!                                                      IndexOfPhysicalPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Point_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  ans = 0_I4B
  DO i = 1, SIZE( Tag )
    ans( i ) = obj % getIndex( [0_I4B, Tag( i )] )
  END DO
END PROCEDURE pn_index_Point_2

!----------------------------------------------------------------------------
!                                                       IndexOfPhysicalCurve
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_curve
  ans = obj % getIndex( [1_I4B, Tag] )
END PROCEDURE pn_index_curve

!----------------------------------------------------------------------------
!                                                      IndexOfPhysicalCurves
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_curve_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  ans = 0_I4B
  DO i = 1, SIZE( Tag )
    ans( i ) = obj % getIndex( [1_I4B, Tag( i )] )
  END DO
END PROCEDURE pn_index_curve_2

!----------------------------------------------------------------------------
!                                                     IndexOfPhysicalSurface
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Surface
  ans = obj % getIndex( [2_I4B, Tag] )
END PROCEDURE pn_index_Surface

!----------------------------------------------------------------------------
!                                                    IndexOfPhysicalSurfaces
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Surface_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  ans = 0_I4B
  DO i = 1, SIZE( Tag )
    ans( i ) = obj % getIndex( [2_I4B, Tag( i )] )
  END DO
END PROCEDURE pn_index_Surface_2

!----------------------------------------------------------------------------
!                                                       IndexOfPhysicalVolume
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Volume
  ans = obj % getIndex( [3_I4B, Tag] )
END PROCEDURE pn_index_Volume

!----------------------------------------------------------------------------
!                                                    IndexOfPhysicalVolumes
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Volume_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  ans = 0_I4B
  DO i = 1, SIZE( Tag )
    ans( i ) = obj % getIndex( [3_I4B, Tag( i )] )
  END DO
END PROCEDURE pn_index_Volume_2

!----------------------------------------------------------------------------
!                                                            OutputFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_output_file
  ans = TRIM( mshFile % FileName % Raw ) // "_" // &
    & TRIM( obj % WhoAmI( indx ) )  &
    // "_" // TRIM( obj % PhysicalName( indx ) % Raw )
END PROCEDURE pn_output_file

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_deallocatedata
  IF( ALLOCATED( obj % NSD ) ) DEALLOCATE( obj % NSD )
  IF( ALLOCATED( obj % Tag ) ) DEALLOCATE( obj % Tag )
  IF( ALLOCATED( obj % numElements ) ) DEALLOCATE( obj % numElements )
  IF( ALLOCATED( obj % numNodes ) ) DEALLOCATE( obj % numNodes )
  IF( ALLOCATED( obj % Entities ) ) DEALLOCATE( obj % Entities )
  IF( ALLOCATED( obj % PhysicalName ) ) DEALLOCATE( obj % PhysicalName )
END PROCEDURE pn_deallocatedata

END SUBMODULE Methods
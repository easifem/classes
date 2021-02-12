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
  CALL Obj % GotoTag( mshFile, ierr )
  !
  IF( .NOT. ierr ) THEN
    READ( mshFile % UnitNo, * ) tp
    IF( ALLOCATED( Obj % NSD ) ) DEALLOCATE( Obj % NSD )
    IF( ALLOCATED( Obj % Tag ) ) DEALLOCATE( Obj % Tag )
    IF( ALLOCATED( Obj % PhysicalName ) ) DEALLOCATE( Obj % PhysicalName )
    IF( ALLOCATED( Obj % numElements ) ) DEALLOCATE( Obj % numElements )
    IF( ALLOCATED( Obj % numNodes ) ) DEALLOCATE( Obj % numNodes )
    ALLOCATE( &
      & Obj % numElements( tp ), &
      & Obj % numNodes( tp ), &
      & Obj % NSD( tp ), &
      & Obj % Tag( tp ), &
      & Obj % PhysicalName( tp ), &
      & Obj % Entities( tp ) &
      & )
    Obj % numElements = 0; Obj % numNodes = 0 ! init
    DO k = 1, tp
      READ( mshFile % UnitNo, *, IOSTAT = IOSTAT ) Obj % NSD( k ), &
        & Obj % Tag( k ), dummystr
      Obj % PhysicalName( k ) = String( dummystr )
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
  tp = SIZE( Obj % NSD )
  !
  IF( PRESENT( Str ) ) THEN
    WRITE( outFile % UnitNo, "(A)" ) TRIM( Str )
  END IF
  !
  WRITE( outFile % UnitNo, "(I6)") SIZE( Obj % NSD )
  !
  DO k = 1, tp
    WRITE( outFile % UnitNo, * ) Obj % NSD( k ), Obj % Tag( k ), &
      & '"'//TRIM( Obj % PhysicalName( k ) % Raw ) // '"'
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
  tSize = Obj % SIZE( )
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
        & String( Str( Obj % NSD( j ), .true. ) ), &
        & String( Str( Obj % Tag( j ), .true. ) ), &
        & TRIM( Obj % PhysicalName( j ) ), &
        & String( Str( Obj % numElements( j ), .true. ) ), &
        & String( Str( Obj % numNodes( j ), .true. ) ) ], " | " )
      WRITE( I, "(A)") TRIM( Str2 ) // " | "
    END DO
    !
    IF( ALLOCATED( Obj % Entities ) ) THEN
      CALL BlankLines( UnitNo = I, NOL = 1 )
      WRITE( I, "(A)" ) "Physical Tag to Entities Tag"
      WRITE( I, "(A)") "| Physical Tag | PhysicalName | Entities Tag |"
      WRITE( I, "(A)") "| :--- | :---: | ---: |"
      DO j = 1, SIZE( Obj % Entities )
        tSize = SIZE( Obj % Entities( j ) )
        WRITE( I, "( A, I4, A, " // TRIM( Str( tSize, .false. ) ) &
          & // "(I4,',')"//", A )" ) &
          & "| ", Obj % Tag( j ), &
          & "| "//TRIM( Obj % PhysicalName( j ) )//" | ", &
          & Obj % Entities( j ) % Val, " |"
      END DO
    END IF
  END IF
END PROCEDURE pn_display

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_get_size
  IF( ALLOCATED( Obj % NSD ) ) THEN
    Ans = SIZE( Obj % NSD )
  ELSE
    Ans = 0
  END IF
END PROCEDURE pn_get_size

!----------------------------------------------------------------------------
!                                                        TotalPhysicalPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_size_point
  IF( ALLOCATED( Obj % NSD ) ) THEN
    Ans = COUNT( Obj % NSD .EQ. 0 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE pn_size_point

!----------------------------------------------------------------------------
!                                                        TotalPhysicalCurves
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_size_Curve
  IF( ALLOCATED( Obj % NSD ) ) THEN
    Ans = COUNT( Obj % NSD .EQ. 1 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE pn_size_Curve

!----------------------------------------------------------------------------
!                                                      TotalPhysicalSurfaces
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_size_Surface
  IF( ALLOCATED( Obj % NSD ) ) THEN
    Ans = COUNT( Obj % NSD .EQ. 2 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE pn_size_Surface

!----------------------------------------------------------------------------
!                                                       TotalPhysicalVolumes
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_size_Volume
  IF( ALLOCATED( Obj % NSD ) ) THEN
    Ans = COUNT( Obj % NSD .EQ. 3 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE pn_size_Volume

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_a
  ! Define internal variables
  INTEGER( I4B ) :: j, tSize

  Ans = 0
  IF( ALLOCATED( Obj % NSD ) ) THEN
    tSize = SIZE( Obj % NSD )
    DO j = 1, tSize
      IF( Obj % PhysicalName( j ) .EQ. TRIM( Name ) ) THEN
        Ans = j
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
  m = SIZE( Name ); Ans = 0
  !
  IF( ALLOCATED( Obj % NSD ) ) THEN
    n = SIZE( Obj % NSD )
    DO i = 1, m
      DO j = 1, n
        IF( Obj % PhysicalName( j ) .EQ. Name( i ) ) THEN
          Ans( i ) = j
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
  tPoints = SIZE( Obj % NSD )
  DO k = 1, tPoints
    IF( Obj % NSD( k ) .EQ. XiDim .AND. Obj % Tag( k ) .EQ. Tag ) THEN
      Ans = k
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
    tPoints = Obj % TotalPhysicalPoints( )
  CASE( 1 )
    tPoints = Obj % TotalPhysicalCurves( )
  CASE( 2 )
    tPoints = Obj % TotalPhysicalSurfaces( )
  CASE( 3 )
    tPoints = Obj % TotalPhysicalVolumes( )
  END SELECT

  ALLOCATE( Ans( tPoints ) )
  tPoints = SIZE( Obj % NSD ); i = 0
  DO k = 1, tPoints
    IF( Obj % NSD( k ) .EQ. XiDimTag ) THEN
      i = i + 1
      Ans( i ) = k
    END IF
  END DO

END PROCEDURE pn_index_d

!----------------------------------------------------------------------------
!                                                          PhysicalPointNames
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Point_names
  ! Define internal variables
  INTEGER( I4B ) :: tPoints, i, k

  tPoints = Obj % TotalPhysicalPoints( )
  ALLOCATE( Ans( tPoints ) )

  tPoints = SIZE( Obj % NSD ); i = 0;
  DO k = 1, tPoints
    IF( Obj % NSD( k ) .EQ. 0 ) THEN
      i = i + 1
      Ans( i ) = Obj % PhysicalName( k ) % Chars( )
    END IF
  END DO
END PROCEDURE pn_Point_names

!----------------------------------------------------------------------------
!                                                          PhysicalCurveNames
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Curve_names
  ! Define internal variables
  INTEGER( I4B ) :: tLines, i, k

  tLines = Obj % TotalPhysicalCurves( )
  ALLOCATE( Ans( tLines ) )

  tLines = SIZE( Obj % NSD ); i = 0;
  DO k = 1, tLines
    IF( Obj % NSD( k ) .EQ. 1 ) THEN
      i = i + 1
      Ans( i ) = Obj % PhysicalName( k ) % Chars( )
    END IF
  END DO
END PROCEDURE pn_Curve_names

!----------------------------------------------------------------------------
!                                                       PhysicalSurfaceNames
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Surface_names
  ! Define internal variables
  INTEGER( I4B ) :: tSurfaces, i, k

  tSurfaces = Obj % TotalPhysicalSurfaces( )
  ALLOCATE( Ans( tSurfaces ) )

  tSurfaces = SIZE( Obj % NSD ); i = 0;
  DO k = 1, tSurfaces
    IF( Obj % NSD( k ) .EQ. 2 ) THEN
      i = i + 1
      Ans( i ) = Obj % PhysicalName( k ) % Chars( )
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
  tVolumes = Obj % TotalPhysicalVolumes( )
  ALLOCATE( Ans( tVolumes ) )
  !
  tVolumes = SIZE( Obj % NSD ); i = 0;
  DO k = 1, tVolumes
    IF( Obj % NSD( k ) .EQ. 3 ) THEN
      i = i + 1
      Ans( i ) = Obj % PhysicalName( k ) % Chars( )
    END IF
  END DO
END PROCEDURE pn_Volume_names

!----------------------------------------------------------------------------
!                                                          PhysicalPointTags
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Point_tags
  ! Define internal variables
  INTEGER( I4B ) :: tPoints, i, k

  tPoints = Obj % TotalPhysicalPoints( )
  ALLOCATE( Ans( tPoints ) )

  tPoints = SIZE( Obj % NSD ); i = 0;
  DO k = 1, tPoints
    IF( Obj % NSD( k ) .EQ. 0 ) THEN
      i = i + 1
      Ans( i ) = Obj % Tag( k )
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
  tLines = Obj % TotalPhysicalCurves( )
  ALLOCATE( Ans( tLines ) )
  !
  tLines = SIZE( Obj % NSD ); i = 0;
  DO k = 1, tLines
    IF( Obj % NSD( k ) .EQ. 1 ) THEN
      i = i + 1
      Ans( i ) = Obj % Tag( k )
    END IF
  END DO
END PROCEDURE pn_Curve_tags

!----------------------------------------------------------------------------
!                                                         PhysicalSurfaceTag
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Surface_tags
  ! Define internal variables
  INTEGER( I4B ) :: tSurfaces, i, k

  tSurfaces = Obj % TotalPhysicalSurfaces( )
  ALLOCATE( Ans( tSurfaces ) )

  tSurfaces = SIZE( Obj % NSD ); i = 0;
  DO k = 1, tSurfaces
    IF( Obj % NSD( k ) .EQ. 2 ) THEN
      i = i + 1
      Ans( i ) = Obj % Tag( k )
    END IF
  END DO
END PROCEDURE pn_Surface_tags

!----------------------------------------------------------------------------
!                                                          PhysicalVolumeTag
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_Volume_tags
  ! Define internal variables
  INTEGER( I4B ) :: tVolumes, i, k

  tVolumes = Obj % TotalPhysicalVolumes( )
  ALLOCATE( Ans( tVolumes ) )

  tVolumes = SIZE( Obj % NSD ); i = 0;
  DO k = 1, tVolumes
    IF( Obj % NSD( k ) .EQ. 3 ) THEN
      i = i + 1
      Ans( i ) = Obj % Tag( k )
    END IF
  END DO
END PROCEDURE pn_Volume_tags

!----------------------------------------------------------------------------
!                                                                 WhoAmI
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_who_am_i
  ! Define internal variables
  INTEGER( I4B ) :: NSD

  NSD = Obj % NSD( I )

  SELECT CASE( NSD )
  CASE( 0 )
    Ans = "PhysicalPoint"
  CASE( 1 )
    Ans = "PhysicalLine"
  CASE( 2 )
    Ans = "PhysicalSurface"
  CASE( 3 )
    Ans = "PhysicalVolume"
  END SELECT
END PROCEDURE pn_who_am_i

!----------------------------------------------------------------------------
!                                                      IndexOfPhysicalPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_point
  Ans = Obj % getIndex( [0_I4B, Tag] )
END PROCEDURE pn_index_point

!----------------------------------------------------------------------------
!                                                      IndexOfPhysicalPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Point_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  Ans = 0_I4B
  DO i = 1, SIZE( Tag )
    Ans( i ) = Obj % getIndex( [0_I4B, Tag( i )] )
  END DO
END PROCEDURE pn_index_Point_2

!----------------------------------------------------------------------------
!                                                       IndexOfPhysicalCurve
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_curve
  Ans = Obj % getIndex( [1_I4B, Tag] )
END PROCEDURE pn_index_curve

!----------------------------------------------------------------------------
!                                                      IndexOfPhysicalCurves
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_curve_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  Ans = 0_I4B
  DO i = 1, SIZE( Tag )
    Ans( i ) = Obj % getIndex( [1_I4B, Tag( i )] )
  END DO
END PROCEDURE pn_index_curve_2

!----------------------------------------------------------------------------
!                                                     IndexOfPhysicalSurface
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Surface
  Ans = Obj % getIndex( [2_I4B, Tag] )
END PROCEDURE pn_index_Surface

!----------------------------------------------------------------------------
!                                                    IndexOfPhysicalSurfaces
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Surface_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  Ans = 0_I4B
  DO i = 1, SIZE( Tag )
    Ans( i ) = Obj % getIndex( [2_I4B, Tag( i )] )
  END DO
END PROCEDURE pn_index_Surface_2

!----------------------------------------------------------------------------
!                                                       IndexOfPhysicalVolume
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Volume
  Ans = Obj % getIndex( [3_I4B, Tag] )
END PROCEDURE pn_index_Volume

!----------------------------------------------------------------------------
!                                                    IndexOfPhysicalVolumes
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_index_Volume_2
  ! Define internal variables
  INTEGER( I4B ) :: i
  Ans = 0_I4B
  DO i = 1, SIZE( Tag )
    Ans( i ) = Obj % getIndex( [3_I4B, Tag( i )] )
  END DO
END PROCEDURE pn_index_Volume_2

!----------------------------------------------------------------------------
!                                                            OutputFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_output_file
  Ans = TRIM( mshFile % FileName % Raw ) // "_" // &
    & TRIM( Obj % WhoAmI( indx ) )  &
    // "_" // TRIM( Obj % PhysicalName( indx ) % Raw )
END PROCEDURE pn_output_file

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE pn_deallocatedata
  IF( ALLOCATED( Obj % NSD ) ) DEALLOCATE( Obj % NSD )
  IF( ALLOCATED( Obj % Tag ) ) DEALLOCATE( Obj % Tag )
  IF( ALLOCATED( Obj % numElements ) ) DEALLOCATE( Obj % numElements )
  IF( ALLOCATED( Obj % numNodes ) ) DEALLOCATE( Obj % numNodes )
  IF( ALLOCATED( Obj % Entities ) ) DEALLOCATE( Obj % Entities )
  IF( ALLOCATED( Obj % PhysicalName ) ) DEALLOCATE( Obj % PhysicalName )
END PROCEDURE pn_deallocatedata

END SUBMODULE Methods
SUBMODULE( gmshMesh_Class ) ConstructorMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_initiate

  ! Define internal variables
  LOGICAL( LGT ) :: ierr

  CALL Display( "READING GMSH FILE")
  CALL Display( "  opening Gmsh file")

  CALL OpenFileToRead( Obj % mshFile, Path, FileName, Extension )
  Obj % NSD = NSD

  CALL Display( "  reading mesh format")
  CALL Obj % Format % ReadFromFile( Obj % mshFile, ierr )

  CALL Display( "  reading physical group information")
  CALL Obj % PhysicalNames % ReadFromFile( Obj % mshFile, ierr )

  CALL Display( "  go to entity tags")
  CALL TypemshEntity % GotoTag( Obj % mshFile, ierr )

  !---------------------------------------------------------------------------
  ! Entities
  !---------------------------------------------------------------------------

  CALL Display( "  reading Entities")

  BLOCK
    INTEGER( I4B ) :: tp, tc, ts, tv, i, j, k, tpt
    INTEGER( I4B ), ALLOCATABLE :: PhysicalTag( : )
    ! we read header of Entities block
    READ( Obj % mshFile % UnitNo, * ) tp, tc, ts, tv
    IF( ALLOCATED( Obj % PointEntities ) ) DEALLOCATE( Obj % PointEntities )
    IF( ALLOCATED( Obj % CurveEntities ) ) DEALLOCATE( Obj % CurveEntities )
    IF( ALLOCATED( Obj % SurfaceEntities ) ) DEALLOCATE(Obj%SurfaceEntities)
    IF( ALLOCATED( Obj % VolumeEntities ) ) DEALLOCATE(Obj%VolumeEntities)
    IF( tp .NE. 0 ) ALLOCATE( Obj % PointEntities( tp ) )
    IF( tc .NE. 0 ) ALLOCATE( Obj % CurveEntities( tc ) )
    IF( ts .NE. 0 ) ALLOCATE( Obj % SurfaceEntities( ts ) )
    IF( tv .NE. 0 ) ALLOCATE( Obj % VolumeEntities( tv ) )

    CALL Display( "  reading point entities" )
    CALL Display( "    creating physical point to entities map" )

    DO i = 1, tp
      CALL Obj % PointEntities( i ) % ReadPointEntity( &
        & Obj % mshFile, .false., ierr )
      ! get total physical tag
      tpt = Obj %  PointEntities( i ) % TotalPhysicalTags( )
      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = Obj % PointEntities( i ) % PhysicalTag
        DO j = 1, tpt
          ! get index of physical tag
          k = Obj % PhysicalNames % IndexOfPhysicalPoint( PhysicalTag( j ) )
          ! append this index to entities
          CALL Append( Obj % PhysicalNames % Entities( k ), [i] )
        END DO
      END IF
    END DO

    CALL Display( "  reading curve entities" )
    CALL Display( "    creating physical curve to entities map" )

    DO i = 1, tc
      CALL Obj % CurveEntities( i ) % ReadCurveEntity( &
      & Obj % mshFile, .false., ierr )
      ! get total physical tag
      tpt = Obj %  CurveEntities( i ) % TotalPhysicalTags( )
      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = Obj % CurveEntities( i ) % PhysicalTag
        DO j = 1, tpt
          ! get index of physical tag
          k = Obj % PhysicalNames % IndexOfPhysicalCurve( PhysicalTag( j ) )
          ! append this index to entities
          CALL Append( Obj % PhysicalNames % Entities( k ), [i] )
        END DO
      END IF
    END DO

    CALL Display( "  reading surface entities" )
    CALL Display( "    creating physical surface to entities map" )

    DO i = 1, ts
      CALL Obj % SurfaceEntities( i ) % ReadSurfaceEntity( Obj % mshFile, &
      & .false., ierr )
      ! get total physical tag
      tpt = Obj %  SurfaceEntities( i ) % TotalPhysicalTags( )

      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = Obj % SurfaceEntities( i ) % PhysicalTag
        DO j = 1, tpt
          ! get index of physical tag
          k = Obj % PhysicalNames % IndexOfPhysicalSurface( PhysicalTag( j ) )
          ! append this index to entities
          CALL Append( Obj % PhysicalNames % Entities( k ), [i] )
        END DO
      END IF
    END DO

    CALL Display( "  reading volume entities" )
    CALL Display( "    creating physical volume to entities map" )

    DO i = 1, tv
      CALL Obj % VolumeEntities( i ) % ReadVolumeEntity( Obj % mshFile, &
      & .false., ierr )
      ! get total physical tag
      tpt = Obj %  VolumeEntities( i ) % TotalPhysicalTags( )
      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = Obj % VolumeEntities( i ) % PhysicalTag
        DO j = 1, tpt
          ! get index of physical tag
          k = Obj % PhysicalNames % IndexOfPhysicalVolume( PhysicalTag( j ) )
          ! append this index to entities
          CALL Append( Obj % PhysicalNames % Entities( k ), [i] )
        END DO
      END IF
    END DO
    IF( ALLOCATED( PhysicalTag ) ) DEALLOCATE( PhysicalTag )
  END BLOCK

  !---------------------------------------------------------------------------
  ! Nodes
  !---------------------------------------------------------------------------

  CALL Display( "  reading Nodes")

  BLOCK
    ! define internal variable
    INTEGER( I4B ) :: i, j, k, l, entityDim, entityTag, parametric, &
      & numNodesInBlock
    INTEGER( I4B ), ALLOCATABLE ::  NodeNumber( : )
    REAL( DFP ), ALLOCATABLE :: NodeCoord( :, : )
    ! we read first line of $Nodes block
    CALL Obj % Nodes % ReadFromFile( Obj % mshFile, Obj % Format, ierr )
    !start reading each entity block
    DO i = 1, Obj % Nodes % numEntityBlocks
      !read entity dimension and entity tag (uid)
      READ( Obj % mshFile % UnitNo, * ) entityDim, entityTag, &
        & parametric, numNodesInBlock
      IF( ALLOCATED( NodeNumber ) ) DEALLOCATE( NodeNumber )
      ALLOCATE( NodeNumber( 1:numNodesInBlock ) )
      ! now we read node numbers in NodeNumber( : )
      DO k = 1, numNodesInBlock
        READ( Obj % mshFile % UnitNo, * ) NodeNumber( k )
      END DO
      IF( ALLOCATED( NodeCoord ) ) DEALLOCATE( NodeCoord )
      ALLOCATE( NodeCoord( 1:3, 1:numNodesInBlock ) )
      ! now we read node coordinates
      DO k = 1, numNodesInBlock
        READ( Obj % mshFile % UnitNo, * ) &
          & (NodeCoord(l, k), l = 1, 3)
      END DO
      !make case based on entity dimension
      SELECT CASE( entityDim )
        CASE( 0 )
          j = getIndex( Obj % PointEntities, entityTag )
          Obj % PointEntities( j ) % NodeNumber = NodeNumber
          Obj % PointEntities( j ) % NodeCoord = NodeCoord
        CASE( 1 )
          j = getIndex( Obj % CurveEntities, entityTag )
          Obj % CurveEntities( j ) % NodeNumber = NodeNumber
          Obj % CurveEntities( j ) % NodeCoord = NodeCoord
        CASE( 2 )
          j = getIndex( Obj % SurfaceEntities, entityTag )
          Obj % SurfaceEntities( j ) % NodeNumber = NodeNumber
          Obj % SurfaceEntities( j ) % NodeCoord = NodeCoord
        CASE( 3 )
          j = getIndex( Obj % VolumeEntities, entityTag )
          Obj % VolumeEntities( j ) % NodeNumber = NodeNumber
          Obj % VolumeEntities( j ) % NodeCoord = NodeCoord
      END SELECT
    END DO
    DEALLOCATE( NodeNumber, NodeCoord )
  END BLOCK

  !---------------------------------------------------------------------------
  ! Elements
  !---------------------------------------------------------------------------

  CALL Display( "  reading Elements")

  ! at this point we have read $Nodes and now we are ready to read elements
  BLOCK
    ! define internal variables
    INTEGER( I4B ) :: i, j, k, l, entityDim, entityTag, elemType, &
      & numElementsInBlock, tNodes, tpt
    INTEGER( I4B ), ALLOCATABLE :: ElemNumber( : ), Nptrs( :, : ), PhyTag( : )
    CALL Obj % Elements % ReadFromFile( Obj % mshFile, Obj % Format, ierr )
    ! start reading each entity block
    DO i = 1, Obj % Elements % numEntityBlocks
      ! read entity dimension and entity tag (uid)
      READ( Obj % mshFile % UnitNo, * ) entityDim, entityTag, ElemType, &
        numElementsInBlock
      ! get the total number of nodes in element
      tNodes = TotalNodesInElement( ElemType )
      IF( ALLOCATED( ElemNumber ) ) DEALLOCATE( ElemNumber )
      ALLOCATE( ElemNumber( numElementsInBlock ) )
      IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
      ALLOCATE( Nptrs( tNodes, numElementsInBlock ) )
      ! now we read ElemNumber and Nptrs
      DO k = 1, numElementsInBlock
        READ( Obj % mshFile % UnitNo, * ) ElemNumber( k ), &
          (Nptrs( l, k ), l = 1, tNodes)
      END DO
      ! make case based on entity dimension
      SELECT CASE( entityDim )
        CASE( 0 )
          j = getIndex( Obj % PointEntities, entityTag )
          ! set the element type
          Obj % PointEntities( j ) % ElemType = ElemType
          Obj % PointEntities( j ) % ElemNumber = ElemNumber
          Obj % PointEntities( j ) % Nptrs = Nptrs
          ! counting nodes in each physical group
          tpt = Obj % PointEntities( j ) % TotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = Obj % PointEntities( j ) % PhysicalTag
            DO k = 1, tpt
              l = Obj % PhysicalNames % IndexOfPhysicalPoint(PhyTag(k))
              Obj % PhysicalNames % numElements( l ) =  &
                & Obj % PhysicalNames % numElements( l ) + numElementsInBlock
            END DO
          END IF
        CASE( 1 )
          j = getIndex( Obj % CurveEntities, entityTag )
          ! set the element type
          Obj % CurveEntities( j ) % ElemType = ElemType
          Obj % CurveEntities( j ) % ElemNumber = ElemNumber
          Obj % CurveEntities( j ) % Nptrs = Nptrs
          ! counting nodes in each physical group
          tpt = Obj % CurveEntities( j ) % TotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = Obj % CurveEntities( j ) % PhysicalTag
            DO k = 1, tpt
              l = Obj % PhysicalNames % IndexOfPhysicalCurve( PhyTag( k ) )
              Obj % PhysicalNames % numElements( l ) =  &
                & Obj % PhysicalNames % numElements( l ) + numElementsInBlock
            END DO
          END IF
        CASE( 2 )
          j = getIndex( Obj % SurfaceEntities, entityTag )
          ! set the element type
          Obj % SurfaceEntities( j ) % ElemType = ElemType
          Obj % SurfaceEntities( j ) % ElemNumber = ElemNumber
          Obj % SurfaceEntities( j ) % Nptrs = Nptrs
          ! counting nodes in each physical group
          tpt = Obj % SurfaceEntities( j ) % TotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = Obj % SurfaceEntities( j ) % PhysicalTag
            DO k = 1, tpt
              l = Obj % PhysicalNames % IndexOfPhysicalSurface( PhyTag( k ) )
              Obj % PhysicalNames % numElements( l ) =  &
                & Obj % PhysicalNames % numElements( l ) + numElementsInBlock
            END DO
          END IF
        CASE( 3 )
          j = getIndex( Obj % VolumeEntities, entityTag )
          ! set the element type
          Obj % VolumeEntities( j ) % ElemType = ElemType
          Obj % VolumeEntities( j ) % ElemNumber = ElemNumber
          Obj % VolumeEntities( j ) % Nptrs = Nptrs
          ! counting nodes in each physical group
          tpt = Obj % VolumeEntities( j ) % TotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = Obj % VolumeEntities( j ) % PhysicalTag
            DO k = 1, tpt
              l = Obj % PhysicalNames % IndexOfPhysicalVolume( PhyTag( k ) )
              Obj % PhysicalNames % numElements( l ) =  &
                & Obj % PhysicalNames % numElements( l ) + numElementsInBlock
            END DO
          END IF
      END SELECT
    END DO
    IF(ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
    IF( ALLOCATED( ElemNumber ) ) DEALLOCATE( ElemNumber )
    IF( ALLOCATED( PhyTag ) ) DEALLOCATE( PhyTag )
  END BLOCK

  !---------------------------------------------------------------------------
  ! Counting number of nodes in physical region
  !---------------------------------------------------------------------------

  CALL Display( "  reading Nodes in Physical groupds")

  BLOCK
    ! define internal variables
    INTEGER( I4B ) :: tpt, i, j, k, tElements
    INTEGER( I4B ), ALLOCATABLE :: Indx( : ), entIndx( : ), Nptrs( : ), &
      & dummyNptrs( : )
    ALLOCATE( Nptrs( Obj % Nodes % maxNodeTag ) )
    ! Points
    tpt = Obj % PhysicalNames % TotalPhysicalPoints( )
    IF( tpt .NE. 0 ) THEN
      Indx = Obj % PhysicalNames % getIndex( 0 )
      ! loop over all physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = ArrayValues( Obj % PhysicalNames % Entities( Indx( i ) ), &
          & TypeIntI4B )
        Obj % PhysicalNames % numNodes( Indx( i ) ) =  &
        Obj % PhysicalNames % numNodes( Indx( i ) ) + SIZE( entIndx )
      END DO
    END IF
    ! Curve
    tpt = Obj % PhysicalNames % TotalPhysicalCurves( )
    IF( tpt .NE. 0 ) THEN
      Indx = Obj % PhysicalNames % getIndex( 1 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = ArrayValues( Obj % PhysicalNames % Entities( Indx( i ) ), &
          & TypeIntI4B )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = Obj % CurveEntities( entIndx( j ) ) % TotalElements( )
          DO k = 1, tElements
            dummyNptrs = Obj % CurveEntities( entIndx( j ) ) % Nptrs( :, k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        Obj % PhysicalNames % numNodes( Indx( i ) ) = COUNT( Nptrs .NE. 0 )
      END DO
    END IF
    ! Surface
    tpt = Obj % PhysicalNames % TotalPhysicalSurfaces( )
    IF( tpt .NE. 0 ) THEN
      Indx = Obj % PhysicalNames % getIndex( 2 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = ArrayValues( Obj % PhysicalNames % Entities( Indx( i ) ), &
          & TypeIntI4B )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = Obj % SurfaceEntities( entIndx( j ) ) % TotalElements( )
          DO k = 1, tElements
            dummyNptrs = Obj % SurfaceEntities( entIndx( j ) ) % Nptrs( :, k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        Obj % PhysicalNames % numNodes( Indx( i ) ) = COUNT( Nptrs .NE. 0 )
      END DO
    END IF

    ! Volume
    tpt = Obj % PhysicalNames % TotalPhysicalVolumes( )
    IF( tpt .NE. 0 ) THEN
      Indx = Obj % PhysicalNames % getIndex( 3 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = ArrayValues( Obj % PhysicalNames % Entities( Indx( i ) ), &
          & TypeIntI4B )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = Obj % VolumeEntities( entIndx( j ) ) % TotalElements( )
          DO k = 1, tElements
            dummyNptrs = Obj % VolumeEntities( entIndx( j ) ) % Nptrs( :, k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        Obj % PhysicalNames % numNodes( Indx( i ) ) = COUNT( Nptrs .NE. 0 )
      END DO
    END IF
    ! add deallocate stmt
    IF( ALLOCATED( Indx ) ) DEALLOCATE( Indx )
    IF( ALLOCATED( entIndx ) ) DEALLOCATE( entIndx )
    IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
    IF( ALLOCATED( DummyNptrs ) ) DEALLOCATE( DummyNptrs )
  END BLOCK

  CALL Display( "  Summary")
  CALL Display( Obj % Format, "## Mesh Format" )
  CALL Display( Obj % PhysicalNames, "## Physical Names" )
  CALL Display( Obj % Nodes, "## Nodes" )
  CALL Display( Obj % Elements, "## Elements" )

END PROCEDURE gmsh_mesh_initiate

!----------------------------------------------------------------------------
!                                                                        msh4
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_constuctor1
  CALL Ans % Initiate( Path, FileName, Extension, NSD )
END PROCEDURE gmsh_mesh_constuctor1

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_deallocatedata
  CALL DeallocateData( Obj % Format )
  CALL DeallocateData( Obj % PhysicalNames )
  CALL DeallocateData( Obj % Nodes )
  CALL DeallocateData( Obj % Elements )
  IF( ALLOCATED( Obj % PointEntities ) ) DEALLOCATE( Obj % PointEntities )
  IF( ALLOCATED( Obj % CurveEntities ) ) DEALLOCATE( Obj % CurveEntities )
  IF( ALLOCATED( Obj % SurfaceEntities ) ) DEALLOCATE( Obj % SurfaceEntities )
  IF( ALLOCATED( Obj % VolumeEntities ) ) DEALLOCATE( Obj % VolumeEntities )
END PROCEDURE gmsh_mesh_deallocatedata
!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_display
  ! Define internal variable
  INTEGER( I4B ) :: I, j
  ! output unit
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  ! print the message
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  ! Printiting the Gmsh Format
  CALL BlankLines( UnitNo = I, NOL = 1 )
  CALL Display( Obj % Format, "Mesh Format = ", I )

  ! Printing the PhysicalNames
  CALL BlankLines( UnitNo = I, NOL = 1 )
  CALL Display( Obj % PhysicalNames, "Physical Names", I )

  ! Printing the point entities
  IF( ALLOCATED( Obj % PointEntities ) ) THEN
    CALL BlankLines( UnitNo = I, NOL = 1 )

    WRITE( I, "(A)" ) "Point Entities"
    DO j = 1, SIZE( Obj % PointEntities )
      CALL Display( &
        & Obj % PointEntities( j ), &
        & "PointEntities( "//TRIM( int2str( j ) )//" )", I )
    END DO
  END IF
  ! Printing the Curve entities
  IF( ALLOCATED( Obj % CurveEntities ) ) THEN
    CALL BlankLines( UnitNo = I, NOL = 1 )
    WRITE( I, "(A)" ) "Curve Entities"
    DO j = 1, SIZE( Obj % CurveEntities )
      CALL Display( &
        & Obj % CurveEntities( j ), &
        & "CurveEntities( "//TRIM( int2str( j ) )//" )", I )
    END DO
  END IF
  ! Printing the Surface entities
  IF ( ALLOCATED( Obj % SurfaceEntities ) ) THEN
    CALL BlankLines( UnitNo = I, NOL = 1 )
    WRITE( I, "(A)" ) "Surface Entities"
    DO j = 1, SIZE( Obj % SurfaceEntities )
      CALL Display( &
        & Obj % SurfaceEntities( j ), &
        & "SurfaceEntities( "//TRIM( int2str( j ) )//" )", I )
    END DO
  END IF
  ! Printing the Volume entities
  IF( ALLOCATED( Obj % VolumeEntities ) ) THEN
    CALL BlankLines( UnitNo = I, NOL = 1 )
    WRITE( I, "(A)" ) "Volume Entities"
    DO j = 1, SIZE( Obj % VolumeEntities )
      CALL Display( &
        & Obj % VolumeEntities( j ), &
        & "VolumeEntities( "//TRIM( int2str( j ) )//" )", I )
    END DO
  END IF
  ! Printing nodes
  CALL BlankLines( UnitNo = I, NOL = 1 )
  CALL Display( Obj % Nodes, "Nodes", I )
  ! Printing elements
  CALL BlankLines( UnitNo = I, NOL = 1 )
  CALL Display( Obj % Elements, "Elements", I )
END PROCEDURE gmsh_mesh_display

END SUBMODULE ConstructorMethods
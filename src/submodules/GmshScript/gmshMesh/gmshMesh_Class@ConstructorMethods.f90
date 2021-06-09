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

  CALL OpenFileToRead( obj % mshFile, Path, FileName, Extension )
  obj % NSD = NSD

  CALL Display( "  reading mesh format")
  CALL obj % Format % ReadFromFile( obj % mshFile, ierr )

  CALL Display( "  reading physical group information")
  CALL obj % PhysicalNames % ReadFromFile( obj % mshFile, ierr )

  CALL Display( "  go to entity tags")
  CALL TypemshEntity % GotoTag( obj % mshFile, ierr )

  !---------------------------------------------------------------------------
  ! Entities
  !---------------------------------------------------------------------------

  CALL Display( "  reading Entities")

  BLOCK
    INTEGER( I4B ) :: tp, tc, ts, tv, i, j, k, tpt
    INTEGER( I4B ), ALLOCATABLE :: PhysicalTag( : )
    ! we read header of Entities block
    READ( obj % mshFile % UnitNo, * ) tp, tc, ts, tv
    IF( ALLOCATED( obj % PointEntities ) ) DEALLOCATE( obj % PointEntities )
    IF( ALLOCATED( obj % CurveEntities ) ) DEALLOCATE( obj % CurveEntities )
    IF( ALLOCATED( obj % SurfaceEntities ) ) DEALLOCATE(obj%SurfaceEntities)
    IF( ALLOCATED( obj % VolumeEntities ) ) DEALLOCATE(obj%VolumeEntities)
    IF( tp .NE. 0 ) ALLOCATE( obj % PointEntities( tp ) )
    IF( tc .NE. 0 ) ALLOCATE( obj % CurveEntities( tc ) )
    IF( ts .NE. 0 ) ALLOCATE( obj % SurfaceEntities( ts ) )
    IF( tv .NE. 0 ) ALLOCATE( obj % VolumeEntities( tv ) )

    CALL Display( "  reading point entities" )
    CALL Display( "    creating physical point to entities map" )

    DO i = 1, tp
      CALL obj % PointEntities( i ) % ReadPointEntity( &
        & obj % mshFile, .false., ierr )
      ! get total physical tag
      tpt = obj %  PointEntities( i ) % TotalPhysicalTags( )
      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = obj % PointEntities( i ) % PhysicalTag
        DO j = 1, tpt
          ! get index of physical tag
          k = obj % PhysicalNames % IndexOfPhysicalPoint( PhysicalTag( j ) )
          ! append this index to entities
          CALL Append( obj % PhysicalNames % Entities( k ), [i] )
        END DO
      END IF
    END DO

    CALL Display( "  reading curve entities" )
    CALL Display( "    creating physical curve to entities map" )

    DO i = 1, tc
      CALL obj % CurveEntities( i ) % ReadCurveEntity( &
      & obj % mshFile, .false., ierr )
      ! get total physical tag
      tpt = obj %  CurveEntities( i ) % TotalPhysicalTags( )
      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = obj % CurveEntities( i ) % PhysicalTag
        DO j = 1, tpt
          ! get index of physical tag
          k = obj % PhysicalNames % IndexOfPhysicalCurve( PhysicalTag( j ) )
          ! append this index to entities
          CALL Append( obj % PhysicalNames % Entities( k ), [i] )
        END DO
      END IF
    END DO

    CALL Display( "  reading surface entities" )
    CALL Display( "    creating physical surface to entities map" )

    DO i = 1, ts
      CALL obj % SurfaceEntities( i ) % ReadSurfaceEntity( obj % mshFile, &
      & .false., ierr )
      ! get total physical tag
      tpt = obj %  SurfaceEntities( i ) % TotalPhysicalTags( )

      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = obj % SurfaceEntities( i ) % PhysicalTag
        DO j = 1, tpt
          ! get index of physical tag
          k = obj % PhysicalNames % IndexOfPhysicalSurface( PhysicalTag( j ) )
          ! append this index to entities
          CALL Append( obj % PhysicalNames % Entities( k ), [i] )
        END DO
      END IF
    END DO

    CALL Display( "  reading volume entities" )
    CALL Display( "    creating physical volume to entities map" )

    DO i = 1, tv
      CALL obj % VolumeEntities( i ) % ReadVolumeEntity( obj % mshFile, &
      & .false., ierr )
      ! get total physical tag
      tpt = obj %  VolumeEntities( i ) % TotalPhysicalTags( )
      IF( tpt .NE. 0 ) THEN
        ! get physical tag int vector
        PhysicalTag = obj % VolumeEntities( i ) % PhysicalTag
        DO j = 1, tpt
          ! get index of physical tag
          k = obj % PhysicalNames % IndexOfPhysicalVolume( PhysicalTag( j ) )
          ! append this index to entities
          CALL Append( obj % PhysicalNames % Entities( k ), [i] )
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
    CALL obj % Nodes % ReadFromFile( obj % mshFile, obj % Format, ierr )
    !start reading each entity block
    DO i = 1, obj % Nodes % numEntityBlocks
      !read entity dimension and entity tag (uid)
      READ( obj % mshFile % UnitNo, * ) entityDim, entityTag, &
        & parametric, numNodesInBlock
      IF( ALLOCATED( NodeNumber ) ) DEALLOCATE( NodeNumber )
      ALLOCATE( NodeNumber( 1:numNodesInBlock ) )
      ! now we read node numbers in NodeNumber( : )
      DO k = 1, numNodesInBlock
        READ( obj % mshFile % UnitNo, * ) NodeNumber( k )
      END DO
      IF( ALLOCATED( NodeCoord ) ) DEALLOCATE( NodeCoord )
      ALLOCATE( NodeCoord( 1:3, 1:numNodesInBlock ) )
      ! now we read node coordinates
      DO k = 1, numNodesInBlock
        READ( obj % mshFile % UnitNo, * ) &
          & (NodeCoord(l, k), l = 1, 3)
      END DO
      !make case based on entity dimension
      SELECT CASE( entityDim )
        CASE( 0 )
          j = getIndex( obj % PointEntities, entityTag )
          obj % PointEntities( j ) % NodeNumber = NodeNumber
          obj % PointEntities( j ) % NodeCoord = NodeCoord
        CASE( 1 )
          j = getIndex( obj % CurveEntities, entityTag )
          obj % CurveEntities( j ) % NodeNumber = NodeNumber
          obj % CurveEntities( j ) % NodeCoord = NodeCoord
        CASE( 2 )
          j = getIndex( obj % SurfaceEntities, entityTag )
          obj % SurfaceEntities( j ) % NodeNumber = NodeNumber
          obj % SurfaceEntities( j ) % NodeCoord = NodeCoord
        CASE( 3 )
          j = getIndex( obj % VolumeEntities, entityTag )
          obj % VolumeEntities( j ) % NodeNumber = NodeNumber
          obj % VolumeEntities( j ) % NodeCoord = NodeCoord
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
    CALL obj % Elements % ReadFromFile( obj % mshFile, obj % Format, ierr )
    ! start reading each entity block
    DO i = 1, obj % Elements % numEntityBlocks
      ! read entity dimension and entity tag (uid)
      READ( obj % mshFile % UnitNo, * ) entityDim, entityTag, ElemType, &
        numElementsInBlock
      ! get the total number of nodes in element
      tNodes = TotalNodesInElement( ElemType )
      IF( ALLOCATED( ElemNumber ) ) DEALLOCATE( ElemNumber )
      ALLOCATE( ElemNumber( numElementsInBlock ) )
      IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
      ALLOCATE( Nptrs( tNodes, numElementsInBlock ) )
      ! now we read ElemNumber and Nptrs
      DO k = 1, numElementsInBlock
        READ( obj % mshFile % UnitNo, * ) ElemNumber( k ), &
          (Nptrs( l, k ), l = 1, tNodes)
      END DO
      ! make case based on entity dimension
      SELECT CASE( entityDim )
        CASE( 0 )
          j = getIndex( obj % PointEntities, entityTag )
          ! set the element type
          obj % PointEntities( j ) % ElemType = ElemType
          obj % PointEntities( j ) % ElemNumber = ElemNumber
          obj % PointEntities( j ) % Nptrs = Nptrs
          ! counting nodes in each physical group
          tpt = obj % PointEntities( j ) % TotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = obj % PointEntities( j ) % PhysicalTag
            DO k = 1, tpt
              l = obj % PhysicalNames % IndexOfPhysicalPoint(PhyTag(k))
              obj % PhysicalNames % numElements( l ) =  &
                & obj % PhysicalNames % numElements( l ) + numElementsInBlock
            END DO
          END IF
        CASE( 1 )
          j = getIndex( obj % CurveEntities, entityTag )
          ! set the element type
          obj % CurveEntities( j ) % ElemType = ElemType
          obj % CurveEntities( j ) % ElemNumber = ElemNumber
          obj % CurveEntities( j ) % Nptrs = Nptrs
          ! counting nodes in each physical group
          tpt = obj % CurveEntities( j ) % TotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = obj % CurveEntities( j ) % PhysicalTag
            DO k = 1, tpt
              l = obj % PhysicalNames % IndexOfPhysicalCurve( PhyTag( k ) )
              obj % PhysicalNames % numElements( l ) =  &
                & obj % PhysicalNames % numElements( l ) + numElementsInBlock
            END DO
          END IF
        CASE( 2 )
          j = getIndex( obj % SurfaceEntities, entityTag )
          ! set the element type
          obj % SurfaceEntities( j ) % ElemType = ElemType
          obj % SurfaceEntities( j ) % ElemNumber = ElemNumber
          obj % SurfaceEntities( j ) % Nptrs = Nptrs
          ! counting nodes in each physical group
          tpt = obj % SurfaceEntities( j ) % TotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = obj % SurfaceEntities( j ) % PhysicalTag
            DO k = 1, tpt
              l = obj % PhysicalNames % IndexOfPhysicalSurface( PhyTag( k ) )
              obj % PhysicalNames % numElements( l ) =  &
                & obj % PhysicalNames % numElements( l ) + numElementsInBlock
            END DO
          END IF
        CASE( 3 )
          j = getIndex( obj % VolumeEntities, entityTag )
          ! set the element type
          obj % VolumeEntities( j ) % ElemType = ElemType
          obj % VolumeEntities( j ) % ElemNumber = ElemNumber
          obj % VolumeEntities( j ) % Nptrs = Nptrs
          ! counting nodes in each physical group
          tpt = obj % VolumeEntities( j ) % TotalPhysicalTags( )
          IF( tpt .NE. 0 ) THEN
            ! get the physical tag in nptrs
            PhyTag = obj % VolumeEntities( j ) % PhysicalTag
            DO k = 1, tpt
              l = obj % PhysicalNames % IndexOfPhysicalVolume( PhyTag( k ) )
              obj % PhysicalNames % numElements( l ) =  &
                & obj % PhysicalNames % numElements( l ) + numElementsInBlock
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
    ALLOCATE( Nptrs( obj % Nodes % maxNodeTag ) )
    ! Points
    tpt = obj % PhysicalNames % TotalPhysicalPoints( )
    IF( tpt .NE. 0 ) THEN
      Indx = obj % PhysicalNames % getIndex( 0 )
      ! loop over all physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = ArrayValues( obj % PhysicalNames % Entities( Indx( i ) ), &
          & TypeIntI4B )
        obj % PhysicalNames % numNodes( Indx( i ) ) =  &
        obj % PhysicalNames % numNodes( Indx( i ) ) + SIZE( entIndx )
      END DO
    END IF
    ! Curve
    tpt = obj % PhysicalNames % TotalPhysicalCurves( )
    IF( tpt .NE. 0 ) THEN
      Indx = obj % PhysicalNames % getIndex( 1 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = ArrayValues( obj % PhysicalNames % Entities( Indx( i ) ), &
          & TypeIntI4B )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = obj % CurveEntities( entIndx( j ) ) % TotalElements( )
          DO k = 1, tElements
            dummyNptrs = obj % CurveEntities( entIndx( j ) ) % Nptrs( :, k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        obj % PhysicalNames % numNodes( Indx( i ) ) = COUNT( Nptrs .NE. 0 )
      END DO
    END IF
    ! Surface
    tpt = obj % PhysicalNames % TotalPhysicalSurfaces( )
    IF( tpt .NE. 0 ) THEN
      Indx = obj % PhysicalNames % getIndex( 2 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = ArrayValues( obj % PhysicalNames % Entities( Indx( i ) ), &
          & TypeIntI4B )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = obj % SurfaceEntities( entIndx( j ) ) % TotalElements( )
          DO k = 1, tElements
            dummyNptrs = obj % SurfaceEntities( entIndx( j ) ) % Nptrs( :, k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        obj % PhysicalNames % numNodes( Indx( i ) ) = COUNT( Nptrs .NE. 0 )
      END DO
    END IF

    ! Volume
    tpt = obj % PhysicalNames % TotalPhysicalVolumes( )
    IF( tpt .NE. 0 ) THEN
      Indx = obj % PhysicalNames % getIndex( 3 )
      ! loop over physical points
      DO i = 1, tpt
        ! get Entities associated
        entIndx = ArrayValues( obj % PhysicalNames % Entities( Indx( i ) ), &
          & TypeIntI4B )
        Nptrs = 0_I4B
        DO j = 1, SIZE( entIndx )
          tElements = obj % VolumeEntities( entIndx( j ) ) % TotalElements( )
          DO k = 1, tElements
            dummyNptrs = obj % VolumeEntities( entIndx( j ) ) % Nptrs( :, k )
            Nptrs( dummyNptrs ) = dummyNptrs
          END DO
        END DO
        ! count the nonzero nptrs
        obj % PhysicalNames % numNodes( Indx( i ) ) = COUNT( Nptrs .NE. 0 )
      END DO
    END IF
    ! add deallocate stmt
    IF( ALLOCATED( Indx ) ) DEALLOCATE( Indx )
    IF( ALLOCATED( entIndx ) ) DEALLOCATE( entIndx )
    IF( ALLOCATED( Nptrs ) ) DEALLOCATE( Nptrs )
    IF( ALLOCATED( DummyNptrs ) ) DEALLOCATE( DummyNptrs )
  END BLOCK

  CALL Display( "  Summary")
  CALL Display( obj % Format, "## Mesh Format" )
  CALL Display( obj % PhysicalNames, "## Physical Names" )
  CALL Display( obj % Nodes, "## Nodes" )
  CALL Display( obj % Elements, "## Elements" )

END PROCEDURE gmsh_mesh_initiate

!----------------------------------------------------------------------------
!                                                                        msh4
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_constuctor1
  CALL ans % Initiate( Path, FileName, Extension, NSD )
END PROCEDURE gmsh_mesh_constuctor1

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_deallocatedata
  CALL DeallocateData( obj % Format )
  CALL DeallocateData( obj % PhysicalNames )
  CALL DeallocateData( obj % Nodes )
  CALL DeallocateData( obj % Elements )
  IF( ALLOCATED( obj % PointEntities ) ) DEALLOCATE( obj % PointEntities )
  IF( ALLOCATED( obj % CurveEntities ) ) DEALLOCATE( obj % CurveEntities )
  IF( ALLOCATED( obj % SurfaceEntities ) ) DEALLOCATE( obj % SurfaceEntities )
  IF( ALLOCATED( obj % VolumeEntities ) ) DEALLOCATE( obj % VolumeEntities )
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
  CALL Display( obj % Format, "Mesh Format = ", I )

  ! Printing the PhysicalNames
  CALL BlankLines( UnitNo = I, NOL = 1 )
  CALL Display( obj % PhysicalNames, "Physical Names", I )

  ! Printing the point entities
  IF( ALLOCATED( obj % PointEntities ) ) THEN
    CALL BlankLines( UnitNo = I, NOL = 1 )

    WRITE( I, "(A)" ) "Point Entities"
    DO j = 1, SIZE( obj % PointEntities )
      CALL Display( &
        & obj % PointEntities( j ), &
        & "PointEntities( "//TRIM( int2str( j ) )//" )", I )
    END DO
  END IF
  ! Printing the Curve entities
  IF( ALLOCATED( obj % CurveEntities ) ) THEN
    CALL BlankLines( UnitNo = I, NOL = 1 )
    WRITE( I, "(A)" ) "Curve Entities"
    DO j = 1, SIZE( obj % CurveEntities )
      CALL Display( &
        & obj % CurveEntities( j ), &
        & "CurveEntities( "//TRIM( int2str( j ) )//" )", I )
    END DO
  END IF
  ! Printing the Surface entities
  IF ( ALLOCATED( obj % SurfaceEntities ) ) THEN
    CALL BlankLines( UnitNo = I, NOL = 1 )
    WRITE( I, "(A)" ) "Surface Entities"
    DO j = 1, SIZE( obj % SurfaceEntities )
      CALL Display( &
        & obj % SurfaceEntities( j ), &
        & "SurfaceEntities( "//TRIM( int2str( j ) )//" )", I )
    END DO
  END IF
  ! Printing the Volume entities
  IF( ALLOCATED( obj % VolumeEntities ) ) THEN
    CALL BlankLines( UnitNo = I, NOL = 1 )
    WRITE( I, "(A)" ) "Volume Entities"
    DO j = 1, SIZE( obj % VolumeEntities )
      CALL Display( &
        & obj % VolumeEntities( j ), &
        & "VolumeEntities( "//TRIM( int2str( j ) )//" )", I )
    END DO
  END IF
  ! Printing nodes
  CALL BlankLines( UnitNo = I, NOL = 1 )
  CALL Display( obj % Nodes, "Nodes", I )
  ! Printing elements
  CALL BlankLines( UnitNo = I, NOL = 1 )
  CALL Display( obj % Elements, "Elements", I )
END PROCEDURE gmsh_mesh_display

END SUBMODULE ConstructorMethods
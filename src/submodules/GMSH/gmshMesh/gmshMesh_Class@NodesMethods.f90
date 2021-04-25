SUBMODULE( gmshMesh_Class ) NodesMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 TotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_totalnodes
  ans = obj % Nodes % numNodes
END PROCEDURE gmsh_mesh_totalnodes

!----------------------------------------------------------------------------
!                                                                    getNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getnodes_array
  ! define internal variables
  INTEGER( I4B ) :: j, k
  !
  ASSOCIATE( minNptrs => obj % Nodes % minNodeTag, &
    & maxNptrs => obj % Nodes % maxNodeTag )
    !
    IF( ALLOCATED( Nodes ) ) DEALLOCATE( Nodes )
    ALLOCATE( Nodes( 1:3, minNptrs : maxNptrs ) )
    Nodes = 0.0_DFP
    ! get nodes from point entities
    IF( ALLOCATED( obj % PointEntities ) ) THEN
      DO j = 1, SIZE( obj % PointEntities )
        IF( ALLOCATED(  obj % PointEntities( j ) %  NodeNumber ) ) THEN
          Nodes( 1:3, obj % PointEntities( j ) % NodeNumber ) = &
            & obj % PointEntities( j ) % NodeCoord( 1:3, : )
        END IF
      END DO
    END IF
    ! get nodes from curve entities
    IF( ALLOCATED( obj % CurveEntities ) ) THEN
      DO j = 1, SIZE( obj % CurveEntities )
        IF( ALLOCATED(  obj % CurveEntities( j ) %  NodeNumber ) ) THEN
          Nodes( 1:3, obj % CurveEntities( j ) % NodeNumber ) = &
            & obj % CurveEntities( j ) % NodeCoord( 1:3, : )
        END IF
      END DO
    END IF
    ! get nodes from surface entities
    IF( ALLOCATED( obj % SurfaceEntities ) ) THEN
      DO j = 1, SIZE( obj % SurfaceEntities )
        IF( ALLOCATED(  obj % SurfaceEntities( j ) %  NodeNumber ) ) THEN
          Nodes( 1:3, obj % SurfaceEntities( j ) % NodeNumber ) = &
            & obj % SurfaceEntities( j ) % NodeCoord( 1:3, : )
        END IF
      END DO
    END IF
    ! get nodes from Volume entities
    IF( ALLOCATED( obj % VolumeEntities ) ) THEN
      DO j = 1, SIZE( obj % VolumeEntities )
        IF( ALLOCATED(  obj % VolumeEntities( j ) %  NodeNumber ) ) THEN
          Nodes( 1:3, obj % VolumeEntities( j ) % NodeNumber ) = &
            & obj % VolumeEntities( j ) % NodeCoord( 1:3, : )
        END IF
      END DO
    END IF
  END ASSOCIATE
END PROCEDURE gmsh_mesh_getnodes_array

!----------------------------------------------------------------------------
!                                                                    getNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getnodes_file
  ! define internal variables
  INTEGER( I4B ) :: j, k, I
  TYPE( File_ ) :: aFile
  !
  IF( PRESENT( unitno ) ) THEN
    I = unitno
  ELSE
    CALL OpenFileToWrite( aFile, &
      & TRIM( obj % mshFile % Path % Raw ), &
      & TRIM( obj % mshFile % FileName % Raw ) // "_Nodes", &
      & TRIM( obj % mshFile % Extension % Raw ) )
    I = aFile % UnitNo
  END IF
  !
  IF( PRESENT( Str ) ) THEN
    WRITE( I, "(A)" ) TRIM( Str )
  END IF
  ! get nodes from point entities
  IF( ALLOCATED( obj % PointEntities ) ) THEN
    DO j = 1, SIZE( obj % PointEntities )
      IF( ALLOCATED(  obj % PointEntities( j ) %  NodeNumber ) ) THEN
        DO k = 1, SIZE( obj % PointEntities( j ) % NodeNumber )
          WRITE( I, "(I6, 3G13.6)" ) &
            & obj % PointEntities( j ) % NodeNumber( k ), &
            & obj % PointEntities( j ) % NodeCoord( 1:3, k )
        END DO
      END IF
    END DO
  END IF
  ! get nodes from Curve entities
  IF( ALLOCATED( obj % CurveEntities ) ) THEN
    DO j = 1, SIZE( obj % CurveEntities )
      IF( ALLOCATED(  obj % CurveEntities( j ) %  NodeNumber ) ) THEN
        DO k = 1, SIZE( obj % CurveEntities( j ) % NodeNumber )
          WRITE( I, "(I6, 3G13.6)" ) &
            & obj % CurveEntities( j ) % NodeNumber( k ), &
            & obj % CurveEntities( j ) % NodeCoord( 1:3, k )
        END DO
      END IF
    END DO
  END IF
  ! get nodes from Surface entities
  IF( ALLOCATED( obj % SurfaceEntities ) ) THEN
    DO j = 1, SIZE( obj % SurfaceEntities )
      IF( ALLOCATED(  obj % SurfaceEntities( j ) %  NodeNumber ) ) THEN
        DO k = 1, SIZE( obj % SurfaceEntities( j ) % NodeNumber )
          WRITE( I, "(I6, 3G13.6)" ) &
            & obj % SurfaceEntities( j ) % NodeNumber( k ), &
            & obj % SurfaceEntities( j ) % NodeCoord( 1:3, k )
        END DO
      END IF
    END DO
  END IF
  ! get nodes from point entities
  IF( ALLOCATED( obj % VolumeEntities ) ) THEN
    DO j = 1, SIZE( obj % VolumeEntities )
      IF( ALLOCATED(  obj % VolumeEntities( j ) %  NodeNumber ) ) THEN
        DO k = 1, SIZE( obj % VolumeEntities( j ) % NodeNumber )
          WRITE( I, "(I6, 3G13.6)" ) &
            & obj % VolumeEntities( j ) % NodeNumber( k ), &
            & obj % VolumeEntities( j ) % NodeCoord( 1:3, k )
        END DO
      END IF
    END DO
  END IF
  IF( PRESENT( EndStr ) ) THEN
    WRITE( I, "(A)" ) TRIM( EndStr )
  END IF
  !
  CALL CloseFile( aFile )
END PROCEDURE gmsh_mesh_getnodes_file
END SUBMODULE NodesMethods
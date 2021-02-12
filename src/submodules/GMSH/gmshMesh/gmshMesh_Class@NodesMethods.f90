SUBMODULE( gmshMesh_Class ) NodesMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 TotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_totalnodes
  Ans = Obj % Nodes % numNodes
END PROCEDURE gmsh_mesh_totalnodes

!----------------------------------------------------------------------------
!                                                                    getNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_getnodes_array
  ! define internal variables
  INTEGER( I4B ) :: j, k
  !
  ASSOCIATE( minNptrs => Obj % Nodes % minNodeTag, &
    & maxNptrs => Obj % Nodes % maxNodeTag )
    !
    IF( ALLOCATED( Nodes ) ) DEALLOCATE( Nodes )
    ALLOCATE( Nodes( 1:3, minNptrs : maxNptrs ) )
    Nodes = 0.0_DFP
    ! get nodes from point entities
    IF( ALLOCATED( Obj % PointEntities ) ) THEN
      DO j = 1, SIZE( Obj % PointEntities )
        IF( ALLOCATED(  Obj % PointEntities( j ) %  NodeNumber ) ) THEN
          Nodes( 1:3, Obj % PointEntities( j ) % NodeNumber ) = &
            & Obj % PointEntities( j ) % NodeCoord( 1:3, : )
        END IF
      END DO
    END IF
    ! get nodes from curve entities
    IF( ALLOCATED( Obj % CurveEntities ) ) THEN
      DO j = 1, SIZE( Obj % CurveEntities )
        IF( ALLOCATED(  Obj % CurveEntities( j ) %  NodeNumber ) ) THEN
          Nodes( 1:3, Obj % CurveEntities( j ) % NodeNumber ) = &
            & Obj % CurveEntities( j ) % NodeCoord( 1:3, : )
        END IF
      END DO
    END IF
    ! get nodes from surface entities
    IF( ALLOCATED( Obj % SurfaceEntities ) ) THEN
      DO j = 1, SIZE( Obj % SurfaceEntities )
        IF( ALLOCATED(  Obj % SurfaceEntities( j ) %  NodeNumber ) ) THEN
          Nodes( 1:3, Obj % SurfaceEntities( j ) % NodeNumber ) = &
            & Obj % SurfaceEntities( j ) % NodeCoord( 1:3, : )
        END IF
      END DO
    END IF
    ! get nodes from Volume entities
    IF( ALLOCATED( Obj % VolumeEntities ) ) THEN
      DO j = 1, SIZE( Obj % VolumeEntities )
        IF( ALLOCATED(  Obj % VolumeEntities( j ) %  NodeNumber ) ) THEN
          Nodes( 1:3, Obj % VolumeEntities( j ) % NodeNumber ) = &
            & Obj % VolumeEntities( j ) % NodeCoord( 1:3, : )
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
      & TRIM( Obj % mshFile % Path % Raw ), &
      & TRIM( Obj % mshFile % FileName % Raw ) // "_Nodes", &
      & TRIM( Obj % mshFile % Extension % Raw ) )
    I = aFile % UnitNo
  END IF
  !
  IF( PRESENT( Str ) ) THEN
    WRITE( I, "(A)" ) TRIM( Str )
  END IF
  ! get nodes from point entities
  IF( ALLOCATED( Obj % PointEntities ) ) THEN
    DO j = 1, SIZE( Obj % PointEntities )
      IF( ALLOCATED(  Obj % PointEntities( j ) %  NodeNumber ) ) THEN
        DO k = 1, SIZE( Obj % PointEntities( j ) % NodeNumber )
          WRITE( I, "(I6, 3G13.6)" ) &
            & Obj % PointEntities( j ) % NodeNumber( k ), &
            & Obj % PointEntities( j ) % NodeCoord( 1:3, k )
        END DO
      END IF
    END DO
  END IF
  ! get nodes from Curve entities
  IF( ALLOCATED( Obj % CurveEntities ) ) THEN
    DO j = 1, SIZE( Obj % CurveEntities )
      IF( ALLOCATED(  Obj % CurveEntities( j ) %  NodeNumber ) ) THEN
        DO k = 1, SIZE( Obj % CurveEntities( j ) % NodeNumber )
          WRITE( I, "(I6, 3G13.6)" ) &
            & Obj % CurveEntities( j ) % NodeNumber( k ), &
            & Obj % CurveEntities( j ) % NodeCoord( 1:3, k )
        END DO
      END IF
    END DO
  END IF
  ! get nodes from Surface entities
  IF( ALLOCATED( Obj % SurfaceEntities ) ) THEN
    DO j = 1, SIZE( Obj % SurfaceEntities )
      IF( ALLOCATED(  Obj % SurfaceEntities( j ) %  NodeNumber ) ) THEN
        DO k = 1, SIZE( Obj % SurfaceEntities( j ) % NodeNumber )
          WRITE( I, "(I6, 3G13.6)" ) &
            & Obj % SurfaceEntities( j ) % NodeNumber( k ), &
            & Obj % SurfaceEntities( j ) % NodeCoord( 1:3, k )
        END DO
      END IF
    END DO
  END IF
  ! get nodes from point entities
  IF( ALLOCATED( Obj % VolumeEntities ) ) THEN
    DO j = 1, SIZE( Obj % VolumeEntities )
      IF( ALLOCATED(  Obj % VolumeEntities( j ) %  NodeNumber ) ) THEN
        DO k = 1, SIZE( Obj % VolumeEntities( j ) % NodeNumber )
          WRITE( I, "(I6, 3G13.6)" ) &
            & Obj % VolumeEntities( j ) % NodeNumber( k ), &
            & Obj % VolumeEntities( j ) % NodeCoord( 1:3, k )
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
SUBMODULE( gmshMesh_Class ) PostProcessMethods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 WriteMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_write_mesh
  TYPE( File_ ) :: afile
  INTEGER( I4B ) :: ii, tsize, te(4), jj

  CALL openFileToWrite(aFile,  Path, FileName, Extension )

  ! Write mesh format
  CALL Write( aFile, '$MeshFormat' )
  CALL Write( aFile, obj%Format%MeshFormat )
  CALL Write( aFile, '$EndMeshFormat' )

  ! Write PhysicalNames
  CALL Write( aFile, '$PhysicalNames' )
  tsize = Obj%PhysicalNames%SIZE()
  CALL Write( aFile, tsize )

  DO ii = 1, tsize
    WRITE( afile%UnitNo, * ) &
      & Obj%PhysicalNames%NSD( ii ), &
      & Obj%PhysicalNames%Tag( ii ), &
      & '"'//TRIM(Obj%PhysicalNames%PhysicalName( ii )%chars()) // '"'
  END DO
  CALL Write( aFile, '$EndPhysicalNames' )

  ! Write Entites
  CALL Write( aFile, '$Entities' )
  te = 0
  IF( ALLOCATED( Obj%PointEntities ) ) te(1) = SIZE(Obj%PointEntities)
  IF( ALLOCATED( Obj%CurveEntities ) ) te(2) = SIZE(Obj%CurveEntities)
  IF( ALLOCATED( Obj%SurfaceEntities ) ) te(3) = SIZE(Obj%SurfaceEntities)
  IF( ALLOCATED( Obj%VolumeEntities ) ) te(4) = SIZE(Obj%VolumeEntities)
  CALL WRITE( aFile, te, row=.true. )
    ! write point entities
  DO ii = 1, te(1)
    tsize=0
    IF( ALLOCATED(Obj%PointEntities(ii)%PhysicalTag) ) THEN
      tsize=SIZE(Obj%PointEntities(ii)%PhysicalTag)
    END IF

    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%PointEntities(ii)%Uid
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%PointEntities(ii)%X
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%PointEntities(ii)%Y
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%PointEntities(ii)%Z
    IF( tsize .EQ. 0 ) THEN
      WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tsize
    ELSE
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) tsize
      CALL WRITE( afile,Obj%PointEntities(ii)%PhysicalTag, row=.true. )
    END IF
  END DO

    ! write curve entities
  DO ii = 1, te(2)
    tsize=0
    IF( ALLOCATED(Obj%CurveEntities(ii)%PhysicalTag) ) THEN
      tsize=SIZE(Obj%CurveEntities(ii)%PhysicalTag)
    END IF

    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%CurveEntities(ii)%Uid
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%CurveEntities(ii)%minX
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%CurveEntities(ii)%minY
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%CurveEntities(ii)%minZ
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%CurveEntities(ii)%maxX
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%CurveEntities(ii)%maxY
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%CurveEntities(ii)%maxZ

    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) tsize
    DO jj = 1, tsize
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
        & Obj%CurveEntities(ii)%PhysicalTag(jj)
    END DO

    tsize = 0
    IF( ALLOCATED(Obj%CurveEntities(ii)%BoundingEntity) ) THEN
      tsize=SIZE(Obj%CurveEntities(ii)%BoundingEntity)
    END IF

    IF( tsize .EQ. 0 ) THEN
      WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tsize
    ELSE
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) tsize
      CALL WRITE( afile,Obj%CurveEntities(ii)%BoundingEntity, .true. )
    END IF
  END DO

    ! write surface entities
  DO ii = 1, te(3)
    tsize=0
    IF( ALLOCATED(Obj%SurfaceEntities(ii)%PhysicalTag) ) THEN
      tsize=SIZE(Obj%SurfaceEntities(ii)%PhysicalTag)
    END IF

    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%Uid
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%minX
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%minY
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%minZ
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%maxX
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%maxY
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%maxZ

    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) tsize
    DO jj = 1, tsize
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
        & Obj%SurfaceEntities(ii)%PhysicalTag(jj)
    END DO

    tsize = 0
    IF( ALLOCATED(Obj%SurfaceEntities(ii)%BoundingEntity) ) THEN
      tsize=SIZE(Obj%SurfaceEntities(ii)%BoundingEntity)
    END IF

    IF( tsize .EQ. 0 ) THEN
      WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tsize
    ELSE
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) tsize
      CALL WRITE( afile,Obj%SurfaceEntities(ii)%BoundingEntity, .true. )
    END IF
  END DO

  ! write volume entities
  DO ii = 1, te(4)
    tsize=0
    IF( ALLOCATED(Obj%VolumeEntities(ii)%PhysicalTag) ) THEN
      tsize=SIZE(Obj%VolumeEntities(ii)%PhysicalTag)
    END IF

    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%VolumeEntities(ii)%Uid
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%VolumeEntities(ii)%minX
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%VolumeEntities(ii)%minY
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%VolumeEntities(ii)%minZ
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%VolumeEntities(ii)%maxX
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%VolumeEntities(ii)%maxY
    WRITE( afile%unitno, FDFP, ADVANCE="NO" ) Obj%VolumeEntities(ii)%maxZ

    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) tsize
    DO jj = 1, tsize
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
        & Obj%VolumeEntities(ii)%PhysicalTag(jj)
    END DO

    tsize = 0
    IF( ALLOCATED(Obj%VolumeEntities(ii)%BoundingEntity) ) THEN
      tsize=SIZE(Obj%VolumeEntities(ii)%BoundingEntity)
    END IF

    IF( tsize .EQ. 0 ) THEN
      WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tsize
    ELSE
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) tsize
      CALL WRITE( afile,Obj%VolumeEntities(ii)%BoundingEntity, .true. )
    END IF
  END DO
  CALL Write( aFile, '$EndEntities' )

  ! Writes $Nodes
  CALL Write( aFile, '$Nodes' )
  WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%Nodes%numEntityBlocks
  WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%Nodes%numNodes
  WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%Nodes%minNodeTag
  WRITE( afile%unitno, FInt32, ADVANCE="YES" ) Obj%Nodes%maxNodeTag
    ! Write Nodes of point entity
  DO ii = 1, te( 1 )
    IF( .NOT. ALLOCATED(Obj%PointEntities(ii)%NodeNumber) ) CYCLE
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%PointEntities(ii)%XiDim
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%PointEntities(ii)%Uid
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) 0
    tSize = 0
    IF( ALLOCATED( Obj%PointEntities(ii)%NodeNumber ) ) THEN
      tSize = SIZE(Obj%PointEntities(ii)%NodeNumber)
    END IF
    WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tSize

    DO jj = 1, tSize
      WRITE( afile%unitno, FInt32, ADVANCE="YES" ) Obj%PointEntities(ii)%NodeNumber(jj)
    END DO
    DO jj = 1, tSize
      CALL WRITE( afile, Nodes(:, Obj%PointEntities(ii)%NodeNumber(jj) ), &
        & row=.true.)
    END DO
  END DO
    ! Write Nodes of curve entity
  DO ii = 1, te( 2 )
    IF( .NOT. ALLOCATED(Obj%CurveEntities(ii)%NodeNumber) ) CYCLE
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%CurveEntities(ii)%XiDim
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%CurveEntities(ii)%Uid
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) 0
    tSize = 0
    IF( ALLOCATED( Obj%CurveEntities(ii)%NodeNumber ) ) THEN
      tSize = SIZE(Obj%CurveEntities(ii)%NodeNumber)
    END IF
    WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tSize

    DO jj = 1, tSize
      WRITE( afile%unitno, FInt32, ADVANCE="YES" ) Obj%CurveEntities(ii)%NodeNumber(jj)
    END DO
    DO jj = 1, tSize
      CALL WRITE( afile, Nodes(:, Obj%CurveEntities(ii)%NodeNumber(jj) ), &
        & row=.true.)
    END DO
  END DO
    ! Write Nodes of surface entity
  DO ii = 1, te( 3 )
    IF( .NOT. ALLOCATED(Obj%SurfaceEntities(ii)%NodeNumber) ) CYCLE
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%XiDim
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%Uid
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) 0
    tSize = 0
    IF( ALLOCATED( Obj%SurfaceEntities(ii)%NodeNumber ) ) THEN
      tSize = SIZE(Obj%SurfaceEntities(ii)%NodeNumber)
    END IF
    WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tSize

    DO jj = 1, tSize
      WRITE( afile%unitno, FInt32, ADVANCE="YES" ) Obj%SurfaceEntities(ii)%NodeNumber(jj)
    END DO
    DO jj = 1, tSize
      CALL WRITE( afile, Nodes(:, Obj%SurfaceEntities(ii)%NodeNumber(jj) ), &
        & row=.true.)
    END DO
  END DO
    ! Write Nodes of volume entity
  DO ii = 1, te( 4 )
    IF( .NOT. ALLOCATED(Obj%VolumeEntities(ii)%NodeNumber) ) CYCLE
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%VolumeEntities(ii)%XiDim
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%VolumeEntities(ii)%Uid
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) 0
    tSize = 0
    IF( ALLOCATED( Obj%VolumeEntities(ii)%NodeNumber ) ) THEN
      tSize = SIZE(Obj%VolumeEntities(ii)%NodeNumber)
    END IF
    WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tSize

    DO jj = 1, tSize
      WRITE( afile%unitno, FInt32, ADVANCE="YES" ) Obj%VolumeEntities(ii)%NodeNumber(jj)
    END DO
    DO jj = 1, tSize
      CALL WRITE( afile, Nodes(:, Obj%VolumeEntities(ii)%NodeNumber(jj) ), &
        & row=.true.)
    END DO
  END DO
  CALL Write( aFile, '$EndNodes' )

    ! Writes $Nodes
  CALL Write( aFile, '$Elements' )
  WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%Elements%numEntityBlocks
  WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%Elements%numElements
  WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%Elements%minElementTag
  WRITE( afile%unitno, FInt32, ADVANCE="YES" ) Obj%Elements%maxElementTag

  ! Write elements of point entity
  DO ii = 1, te( 1 )
    IF( .NOT. ALLOCATED(Obj%PointEntities(ii)%ElemNumber) ) CYCLE
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%PointEntities(ii)%XiDim
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%PointEntities(ii)%Uid
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%PointEntities(ii)%ElemType
    tSize = 0
    IF( ALLOCATED( Obj%PointEntities(ii)%ElemNumber ) ) THEN
      tSize = SIZE(Obj%PointEntities(ii)%ElemNumber)
    END IF
    WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tSize

    DO jj = 1, tSize
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
        & Obj%PointEntities(ii)%ElemNumber(jj)
      CALL WRITE( afile, Obj%PointEntities(ii)%Nptrs(:, jj), row=.true.)
    END DO
  END DO

  ! Write elements of curve entity
  DO ii = 1, te( 2 )
    IF( .NOT. ALLOCATED(Obj%CurveEntities(ii)%ElemNumber) ) CYCLE
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%CurveEntities(ii)%XiDim
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%CurveEntities(ii)%Uid
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
      & Obj%CurveEntities(ii)%ElemType
    tSize = 0
    IF( ALLOCATED( Obj%CurveEntities(ii)%ElemNumber ) ) THEN
      tSize = SIZE(Obj%CurveEntities(ii)%ElemNumber)
    END IF
    WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tSize

    DO jj = 1, tSize
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
        & Obj%CurveEntities(ii)%ElemNumber(jj)
      CALL WRITE( afile, Obj%CurveEntities(ii)%Nptrs(:, jj), row=.true.)
    END DO
  END DO

  ! Write elements of surface entity
  DO ii = 1, te( 3 )
    IF( .NOT. ALLOCATED(Obj%SurfaceEntities(ii)%ElemNumber) ) CYCLE
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%XiDim
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%SurfaceEntities(ii)%Uid
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
      & Obj%SurfaceEntities(ii)%ElemType
    tSize = 0
    IF( ALLOCATED( Obj%SurfaceEntities(ii)%ElemNumber ) ) THEN
      tSize = SIZE(Obj%SurfaceEntities(ii)%ElemNumber)
    END IF
    WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tSize

    DO jj = 1, tSize
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
        & Obj%SurfaceEntities(ii)%ElemNumber(jj)
      CALL WRITE( afile, Obj%SurfaceEntities(ii)%Nptrs(:, jj), row=.true.)
    END DO
  END DO

  ! Write elements of volume entity
  DO ii = 1, te( 4 )
    IF( .NOT. ALLOCATED(Obj%VolumeEntities(ii)%ElemNumber) ) CYCLE
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%VolumeEntities(ii)%XiDim
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) Obj%VolumeEntities(ii)%Uid
    WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
      & Obj%VolumeEntities(ii)%ElemType
    tSize = 0
    IF( ALLOCATED( Obj%VolumeEntities(ii)%ElemNumber ) ) THEN
      tSize = SIZE(Obj%VolumeEntities(ii)%ElemNumber)
    END IF
    WRITE( afile%unitno, FInt32, ADVANCE="YES" ) tSize

    DO jj = 1, tSize
      WRITE( afile%unitno, FInt32, ADVANCE="NO" ) &
        & Obj%VolumeEntities(ii)%ElemNumber(jj)
      CALL WRITE( afile, Obj%VolumeEntities(ii)%Nptrs(:, jj), .true.)
    END DO
  END DO
  CALL Write( aFile, '$EndElements' )
  CALL CloseFile(aFile)

END PROCEDURE gmsh_mesh_write_mesh

!----------------------------------------------------------------------------
!                                                            WriteNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_write_nodedata_1
  integer( I4B ) :: iname, ttime, tspace, unitno, tsize, is, ie, i, itime
  character( LEN = 200 ) :: path0, path, filename, cmd
  character( LEN = 50 ) :: stepno, ts
  logical( LGT ) :: spacecompo
  type( file_ ) :: afile
  real( dfp ), allocatable :: vals( : )
  integer( i4b ), allocatable :: dofs( : )
  real( dfp ) :: val( 3 )

  iname = IndexOf( dofObj, Name )

  if( size( indx ) .eq. 1 ) then
    stepno = "_"//trim( int2str( indx( 1 ) ) )
    ts = "/TimeStep/"
  else
    stepno = "_"//trim( int2str( indx( 2 ) ) )
    ts = "/Iteration/TimeStep_"//trim( int2str( indx( 1 ) ) )//"/"
  end if

  if( dofobj % map( iname, 2 ) .eq. -1 ) then
    spacecompo = .false.
    tspace = 1
    path0 = TRIM( Obj % mshFile % Path ) // "/GMSH/NodeData/Scalar/"
  else if( dofobj % map( iname, 2 ) .gt. 0 ) then
    spacecompo = .true.
    tspace = dofobj % map( iname, 2 )
    path0 = TRIM( Obj % mshFile % Path ) // "/GMSH/NodeData/Vector/"
  end if

  !<--
  !<-- now we need to get the space-components
  tsize = dofobj % map( iname+1, 5 ) - dofobj % map( iname, 5 )
  allocate( dofs( tsize ) ); val = 0.0_dfp
  do i = 0, tsize-1
    dofs( i+1 ) = dofobj % map( iname, 5 ) + i
  end do

!<--- only space nodal values
  if( dofobj % map( iname, 3 ) .eq. 1 ) then

    ttime = 0
    itime = 1
    path = trim( path0 ) // trim( name ) //  trim(ts) // "/"
      !! /GMSH/NodeData/Scalar/VarName/TimeStep/
      !! /GMSH/NodeData/Vector/VarName/TimeStep/
      !! /GMSH/NodeData/Scalar/VarName/Iteration/TimeStep_xx
      !! /GMSH/NodeData/Vector/VarName/Iteration/TimeStep_xx

    filename = trim( path ) // trim( name ) // trim( stepno )

#include "./nodedata.inc"

!<--- both space and time nodal values
  else if( dofobj % map( iname, 3 ) .gt. 1 ) then

    ttime = dofobj % map( iname, 3 )
    itime = 1

    do itime = 1, ttime

      path = trim( path0 ) // trim( name ) // trim( int2str(itime) ) &
        & // trim(ts) // "/"

      filename = trim( path ) // trim( name ) &
        & // trim( int2str(itime) ) // trim( stepno )
#include "nodedata.inc"
    end do
  end if
END PROCEDURE gmsh_mesh_write_nodedata_1

!----------------------------------------------------------------------------
!                                                              WriteNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE gmsh_mesh_write_nodedata_2
  CHARACTER( LEN = 1 ), ALLOCATABLE :: names( : )
  INTEGER( I4B ) :: ii, n

  names = .names. dofobj
  n = SIZE( names )

  IF( PRESENT(nodes) ) THEN
    DO ii = 1, n
      CALL obj % gmsh_mesh_write_nodedata_1( x = x, dofobj = dofobj, &
        & name = names( ii ), indx = indx, local_nptrs=local_nptrs, &
        & nodes = nodes )
    END DO
  ELSE
    DO ii = 1, n
      CALL obj % gmsh_mesh_write_nodedata_1( x = x, dofobj = dofobj, &
        & name = names( ii ), indx = indx, local_nptrs=local_nptrs )
    END DO
  END IF
  IF( ALLOCATED( names ) ) DEALLOCATE( names )

END PROCEDURE gmsh_mesh_write_nodedata_2

END SUBMODULE PostProcessMethods
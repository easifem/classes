MODULE gmshModel_Class
  !! This class is container for all gmsh model

USE BaseType
USE GlobalData
USE BaseMethod
USE gmshGeo_Class
USE gmshMesh_Class

IMPLICIT NONE

INTEGER( I4B ), PARAMETER :: def_point_phy2ent = 100
INTEGER( I4B ), PARAMETER :: def_curve_phy2ent = 100
INTEGER( I4B ), PARAMETER :: def_surface_phy2ent = 100
INTEGER( I4B ), PARAMETER :: def_volume_phy2ent = 100
INTEGER( I4B ), PARAMETER :: def_point_ent2phy = 100
INTEGER( I4B ), PARAMETER :: def_curve_ent2phy = 100
INTEGER( I4B ), PARAMETER :: def_surface_ent2phy = 100
INTEGER( I4B ), PARAMETER :: def_volume_ent2phy = 100

PRIVATE

!----------------------------------------------------------------------------
!                                                                  gmshModel_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This data type is container for all the gmsh models

TYPE :: gmshModel_

  TYPE( Buffer_ ), POINTER :: buffer => NULL( )

  CHARACTER( LEN = 5 ) :: name = ""
    !! We can use this file two take backups and write content of model
  TYPE( gmshGeo_ ), POINTER :: geo => NULL( )
    !! geometry due to internal kernel

    ! TYPE( gmshOCC_ ), POINTER :: occ => NULL( )

    !! This experimental feature
  TYPE( gmshMesh_ ), POINTER :: mesh => NULL( )

  TYPE( String ), ALLOCATABLE :: EntityPointName( : )
  TYPE( String ), ALLOCATABLE :: EntityCurveName( : )
  TYPE( String ), ALLOCATABLE :: EntitySurfaceName( : )
  TYPE( String ), ALLOCATABLE :: EntityVolumeName( : )

  TYPE( String ), ALLOCATABLE :: PhysicalPointName( : )
  TYPE( String ), ALLOCATABLE :: PhysicalCurveName( : )
  TYPE( String ), ALLOCATABLE :: PhysicalSurfaceName( : )
  TYPE( String ), ALLOCATABLE :: PhysicalVolumeName( : )

  INTEGER( I4B ), ALLOCATABLE :: PhysicalPointUID( : )
  INTEGER( I4B ), ALLOCATABLE :: PhysicalCurveUID( : )
  INTEGER( I4B ), ALLOCATABLE :: PhysicalSurfaceUID( : )
  INTEGER( I4B ), ALLOCATABLE :: PhysicalVolumeUID( : )

  TYPE( IntVectorPointer_ ), ALLOCATABLE :: Point_PhysicalToEntity( : )
  TYPE( IntVectorPointer_ ), ALLOCATABLE :: Curve_PhysicalToEntity( : )
  TYPE( IntVectorPointer_ ), ALLOCATABLE :: Surface_PhysicalToEntity( : )
  TYPE( IntVectorPointer_ ), ALLOCATABLE :: Volume_PhysicalToEntity( : )

  TYPE( IntVectorPointer_ ), ALLOCATABLE :: Point_EntityToPhysical( : )
  TYPE( IntVectorPointer_ ), ALLOCATABLE :: Curve_EntityToPhysical( : )
  TYPE( IntVectorPointer_ ), ALLOCATABLE :: Surface_EntityToPhysical( : )
  TYPE( IntVectorPointer_ ), ALLOCATABLE :: Volume_EntityToPhysical( : )


  TYPE( gmshModel_ ), POINTER :: next => NULL( )
  !! Pointer to the next model for linked list
  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: add => model_add
    ! PROCEDURE, PUBLIC, PASS( obj ) :: remove => model_remove
    ! PROCEDURE, PUBLIC, PASS( obj ) :: list => model_list
    ! PROCEDURE, PUBLIC, PASS( obj ) :: getCurrent => model_getCurrent
    ! PROCEDURE, PUBLIC, PASS( obj ) :: setCurrent => model_setCurrent
    PROCEDURE, PUBLIC, PASS( obj ) :: write => model_write

    PROCEDURE, PUBLIC, PASS( obj ) :: getEntities => model_getEntities
    PROCEDURE, PUBLIC, PASS( obj ) :: setEntityName => model_setEntityName
    PROCEDURE, PUBLIC, PASS( obj ) :: getEntityName => model_getEntityName

    PROCEDURE, PUBLIC, PASS( obj ) :: addPhysicalGroup => &
      & model_addPhysicalGroup

    PROCEDURE, PUBLIC, PASS( obj ) :: getPhysicalGroups => &
      & model_getPhysicalGroups

    PROCEDURE, PUBLIC, PASS( obj ) :: getEntitiesForPhysicalGroup => &
      & model_getEntitiesForPhysicalGroup

    PROCEDURE, PUBLIC, PASS( obj ) :: getPhysicalGroupsForEntity => &
      & model_getPhysicalGroupsForEntity

    PROCEDURE, PUBLIC, PASS( obj ) :: setPhysicalName => &
      & model_setPhysicalName

    PROCEDURE, PUBLIC, PASS( obj ) :: getPhysicalName => &
      & model_getPhysicalName

END TYPE gmshModel_

PUBLIC :: gmshModel_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION model_write( obj, UnitNo ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: UnitNo
  INTEGER( I4B ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii
  ans = 0

  IF( .NOT. ASSOCIATED ( obj % geo ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        Model_write()")
    CALL Display( "          obj % geo not allocated")
    STOP
  END IF

  ! CALL Display( "gmsh%model:: calling gmsh%model%geo%write()" )
  ans = obj % geo % write( UnitNo )

  IF( ASSOCIATED( obj % buffer ) ) THEN
    ! CALL Display( "gmsh%model:: writing gmsh%model%buffer()" )
    DO ii = 1, obj % Buffer % tLine
      WRITE( UnitNo, "(DT)" ) obj % Buffer % Line( ii ) % Ptr
    END DO
  END IF

  IF( ASSOCIATED( obj % geo % mesh ) ) THEN
    ! CALL Display( "gmsh%model:: calling gmsh%model%geo%mesh%write()" )
    ans = obj % geo % mesh % write( UnitNo )
  END IF

  IF( ASSOCIATED( obj % mesh ) ) THEN
    ! CALL Display( "gmsh%model:: calling gmsh%model%mesh%write()" )
    ans = obj % mesh % write( UnitNo )
  END IF

END FUNCTION model_write

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function add the model
! Currently only one model can be added

FUNCTION model_add( obj, Name ) RESULT( ans )
  CLASS( gmshModel_  ), TARGET, INTENT( INOUT) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Name
  INTEGER( I4B ) :: ans


  CALL Display( "gmsh%model:: Model " // trim( Name ) // " added" )
  ans = 0
  CALL Display( "gmsh%model:: setting name of Model" )
  obj % name = trim( name )

  obj % next => NULL( )

  ! model % geo
  CALL Display( "gmsh%model:: allocating obj%geo" )
  ALLOCATE( obj % geo )

  ! model model % geo % mesh
  CALL Display( "gmsh%model:: allocating obj%geo%mesh" )
  ALLOCATE( obj % geo % mesh )

  ! allocate obj % geo % buffer
  CALL Display( "gmsh%model:: allocating obj%geo%buffer" )
  ALLOCATE( obj % geo % buffer )

  ! model % mesh
  CALL Display( "gmsh%model:: allocating obj%mesh" )
  ALLOCATE( obj % mesh )

  !! model_name_buffer.txt
  CALL Display( "gmsh%model:: allocating obj%buffer" )
  ALLOCATE( obj % buffer )

END FUNCTION model_add

!----------------------------------------------------------------------------
!                                                                 getEntities
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Get all the entities in the current model. If dim is >= 0, return only
! the entities of the specified dimension (e.g. points if dim == 0).
! The entities are returned as a vector of (dim, tag) integer pairs.

FUNCTION model_getEntities( obj, dim ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), ALLOCATABLE :: ans( :, : )

  ! internal variables
  INTEGER( I4B ) :: ii, n, jj

  IF( .NOT. ASSOCIATED( obj % geo ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_getEntities()")
    CALL Display( "          obj % geo not associated")
    STOP
  END IF

  SELECT CASE( dim )

  CASE( 3 )

    IF( obj % geo % tVolumes .NE. 0 ) THEN
      n = obj % geo % tVolumes
      ALLOCATE( ans( n, 2 ) )
      ans = 0
      DO ii = 1, n
        ans( ii, 1 ) = dim
        ans( ii, 2 ) = obj % geo % Volume( ii ) % Ptr % Uid
      END DO
    ELSE
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getEntities()")
      CALL Display( "          obj % geo % Volume not associated")
      STOP
    END IF

  CASE( 2 )

    IF( obj % geo % tSurfaces .NE. 0 ) THEN
      n = obj % geo % tSurfaces
      ALLOCATE( ans( n, 2 ) )
      ans = 0
      DO ii = 1, n
        ans( ii, 1 ) = dim
        ans( ii, 2 ) = obj % geo % Surface( ii ) % Ptr % Uid
      END DO
    ELSE
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getEntities()")
      CALL Display( "          obj % geo % Surface not associated")
      STOP
    END IF

  CASE( 1 )

    IF( obj % geo % tCurves .NE. 0 ) THEN
      n = obj % geo % tCurves
      ALLOCATE( ans( n, 2 ) )
      ans = 0
      DO ii = 1, n
        ans( ii, 1 ) = dim
        ans( ii, 2 ) = obj % geo % Curve( ii ) % Ptr % Uid
      END DO
    ELSE
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getEntities()")
      CALL Display( "          obj % geo % Curve not associated")
      STOP
    END IF

  CASE( 0 )

    IF( obj % geo % tPoints .NE. 0 ) THEN
      n = obj % geo % tPoints
      ALLOCATE( ans( n, 2 ) )
      ans = 0
      DO ii = 1, n
        ans( ii, 1 ) = dim
        ans( ii, 2 ) = obj % geo % Point( ii ) % Ptr % Uid
      END DO
    ELSE
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getEntities()")
      CALL Display( "          obj % geo % Point not associated")
      STOP
    END IF

  CASE DEFAULT

    n = obj % geo % tPoints + obj % geo % tCurves &
      & + obj % geo % tSurfaces + obj % geo % tVolumes

    IF( n .NE. 0 ) THEN
      ALLOCATE( ans( n, 2 ) )
    ELSE
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getEntities()")
      CALL Display( "          obj % geo seems empty")
      STOP
    END IF

    DO ii = 1, obj % geo % tPoints
      ans( ii, 1 ) = 0
      ans( ii, 2 ) = obj % geo % Point( ii ) % Ptr % Uid
    END DO

    DO ii = 1, obj % geo % tCurves
      jj = ii + obj % geo % tPoints
      ans( jj, 1 ) = 1
      ans( jj, 2 ) = obj % geo % Curve( ii ) % Ptr % Uid
    END DO

    DO ii = 1, obj % geo % tSurfaces
      jj = ii + obj % geo % tPoints + obj % geo % tCurves
      ans( jj, 1 ) = 2
      ans( jj, 2 ) = obj % geo % Surface( ii ) % Ptr % Uid
    END DO

    DO ii = 1, obj % geo % tVolumes

      jj = ii + obj % geo % tPoints + obj % geo % tCurves &
        + obj % geo % tSurfaces

      ans( jj, 1 ) = 3
      ans( jj, 2 ) = obj % geo % Volume( ii ) % Ptr % Uid

    END DO

  END SELECT

END FUNCTION model_getEntities

!----------------------------------------------------------------------------
!                                                              setEntityName
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set the name of the entity of dimension dim and tag tag

FUNCTION model_setEntityName( obj, dim, tag, name ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  CHARACTER( LEN = * ),  INTENT( IN ) ::  name
  INTEGER( I4B ) :: ans

  !internal variables
  INTEGER( I4B ) :: ii

  SELECT CASE( dim )

  CASE( 0 )

    DO ii = 1, obj % geo % tPoints
      IF( obj % geo % point( ii ) % ptr % uid .EQ. tag ) THEN
        obj % EntityPointName( ii ) = name
      END IF
    END DO

  CASE( 1 )

    DO ii = 1, obj % geo % tCurves
      IF( obj % geo % Curve( ii ) % ptr % uid .EQ. tag ) THEN
        obj % EntityCurveName( ii ) = name
      END IF
    END DO

  CASE( 2 )

    DO ii = 1, obj % geo % tSurfaces
      IF( obj % geo % Surface( ii ) % ptr % uid .EQ. tag ) THEN
        obj % EntitySurfaceName( ii ) = name
      END IF
    END DO

  CASE( 3 )

    DO ii = 1, obj % geo % tVolumes
      IF( obj % geo % Volume( ii ) % ptr % uid .EQ. tag ) THEN
        obj % EntityVolumeName( ii ) = name
      END IF
    END DO

  END SELECT

END FUNCTION model_setEntityName

!----------------------------------------------------------------------------
!                                                              getEntityName
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! get the name of the entity of dimension dim and tag tag

FUNCTION model_getEntityName( obj, dim, tag ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  TYPE( String ) ::  ans

  !internal variables
  INTEGER( I4B ) :: ii

  SELECT CASE( dim )

  CASE( 0 )

    DO ii = 1, obj % geo % tPoints
      IF( obj % geo % point( ii ) % ptr % uid .EQ. tag ) THEN
        ans = obj % EntityPointName( ii )
      END IF
    END DO

  CASE( 1 )

    DO ii = 1, obj % geo % tCurves
      IF( obj % geo % Curve( ii ) % ptr % uid .EQ. tag ) THEN
        ans = obj % EntityCurveName( ii )
      END IF
    END DO

  CASE( 2 )

    DO ii = 1, obj % geo % tSurfaces
      IF( obj % geo % Surface( ii ) % ptr % uid .EQ. tag ) THEN
        ans = obj % EntitySurfaceName( ii )
      END IF
    END DO

  CASE( 3 )

    DO ii = 1, obj % geo % tVolumes
      IF( obj % geo % Volume( ii ) % ptr % uid .EQ. tag ) THEN
        ans = obj % EntityVolumeName( ii )
      END IF
    END DO

  END SELECT

END FUNCTION model_getEntityName

!----------------------------------------------------------------------------
!                                                           addPhysicalGroup
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Add a physical group of dimension dim, grouping the model entities with
! tags tags. Return the tag of the physical group, equal to tag if tag is
! positive, or a new tag if tag < 0.

FUNCTION model_addPhysicalGroup( obj, dim, tags, uid ) RESULT( ans )
  CLASS( gmshModel_  ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: tags( : )
  INTEGER( I4B ), INTENT( IN ) :: uid
  INTEGER( I4B ) :: ans

  ! internal variables
  INTEGER( I4B ) :: ii, n, te2p, tp2e, jj
  TYPE( String ), ALLOCATABLE :: s( : )
  TYPE( String ) :: ss

  IF( uid .LT. 0 ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          negative uid not supported")
    STOP
  END IF

  ! internal variables
  SELECT CASE( dim )

  CASE( 0 )

    ! CALL Display( "gmsh%model:: adding physical point")

#include "./addphysicalpoint.inc"

  CASE( 1 )

    ! CALL Display( "gmsh%model:: adding physical curve")

#include "./addphysicalcurve.inc"

  CASE( 2 )

    ! CALL Display( "gmsh%model:: adding physical surface")

#include "./addphysicalsurface.inc"

  CASE( 3 )

    ! CALL Display( "gmsh%model:: adding physical volume")

#include "./addphysicalvolume.inc"

  END SELECT

END FUNCTION model_addPhysicalGroup

!----------------------------------------------------------------------------
!                                                          getPhysicalGroups
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Get all the physical groups in the current model. If dim is >= 0, return
! only the entities of the specified dimension (e.g. physical points
! if dim == 0). The entities are returned as a vector of (dim, tag)
! integer pairs.

FUNCTION model_getPhysicalGroups( obj, dim ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), ALLOCATABLE :: ans( :,: )


  ! internal variables
  INTEGER( I4B ) :: ii, n, jj, i( 0:3 )

  SELECT CASE( dim )

  CASE( 3 )

    IF( .NOT. ALLOCATED( obj % PhysicalVolumeUID ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getPhysicalGroups()")
      CALL Display( "          obj % PhysicalVolumeUID not allocated")
      CALL Display( "          Seems no Physical Volume present")
      STOP
    END IF

    n = SIZE( obj % PhysicalVolumeUID )
    ALLOCATE( ans( n, 2 ) )
    ans = 0
    DO ii = 1, n
      ans( ii, 1 ) = dim
      ans( ii, 2 ) = obj % PhysicalVolumeUID( ii )
    END DO

  CASE( 2 )

    IF( .NOT. ALLOCATED( obj % PhysicalSurfaceUID ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getPhysicalGroups()")
      CALL Display( "          obj % PhysicalSurfaceUID not allocated")
      CALL Display( "          Seems no Physical Surface present")
      STOP
    END IF

    n = SIZE( obj % PhysicalSurfaceUID )
    ALLOCATE( ans( n, 2 ) )
    ans = 0
    DO ii = 1, n
      ans( ii, 1 ) = dim
      ans( ii, 2 ) = obj % PhysicalSurfaceUID( ii )
    END DO

  CASE( 1 )

    IF( .NOT. ALLOCATED( obj % PhysicalCurveUID ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getPhysicalGroups()")
      CALL Display( "          obj % PhysicalCurveUID not allocated")
      CALL Display( "          Seems no Physical Curve present")
      STOP
    END IF

    n = SIZE( obj % PhysicalCurveUID )
    ALLOCATE( ans( n, 2 ) )
    ans = 0
    DO ii = 1, n
      ans( ii, 1 ) = dim
      ans( ii, 2 ) = obj % PhysicalCurveUID( ii )
    END DO

  CASE( 0 )

    IF( .NOT. ALLOCATED( obj % PhysicalPointUID ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getPhysicalGroups()")
      CALL Display( "          obj % PhysicalPointUID not allocated")
      CALL Display( "          Seems no Physical Point present")
      STOP
    END IF

    n = SIZE( obj % PhysicalPointUID )
    ALLOCATE( ans( n, 2 ) )
    ans = 0
    DO ii = 1, n
      ans( ii, 1 ) = dim
      ans( ii, 2 ) = obj % PhysicalPointUID( ii )
    END DO

  CASE DEFAULT

    i = 0
    IF( ALLOCATED( obj % PhysicalPointUID ) ) THEN
      i( 0 ) = SIZE( obj % PhysicalPointUID )
    END IF

    IF( ALLOCATED( obj % PhysicalCurveUID ) ) THEN
      i( 1 ) = SIZE( obj % PhysicalCurveUID )
    END IF

    IF( ALLOCATED( obj % PhysicalSurfaceUID ) ) THEN
      i( 2 ) = SIZE( obj % PhysicalSurfaceUID )
    END IF

    IF( ALLOCATED( obj % PhysicalVolumeUID ) ) THEN
      i( 3 ) = SIZE( obj % PhysicalVolumeUID )
    END IF

    n = SUM( i )

    IF( n .NE. 0 ) THEN
      ALLOCATE( ans( n, 2 ) )
    ELSE
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "        model_getEntities()")
      CALL Display( "          Seems no Physical groupd defined")
      STOP
    END IF

    DO ii = 1, i( 0 )
      ans( ii, 1 ) = 0
      ans( ii, 2 ) = obj % PhysicalPointUID( ii )
    END DO

    DO ii = 1, i( 1 )
      jj = ii + i( 0 )
      ans( jj, 1 ) = 1
      ans( jj, 2 ) = obj % PhysicalCurveUID( ii )
    END DO

    DO ii = 1, i( 2 )
      jj = ii + i( 0 ) + i( 1 )
      ans( jj, 1 ) = 2
      ans( jj, 2 ) = obj % PhysicalSurfaceUID( ii )
    END DO

    DO ii = 1, i( 3 )
      jj = ii + SUM( i( 0:2 ) )
      ans( jj, 1 ) = 3
      ans( jj, 2 ) = obj % PhysicalVolumeUID( ii )
    END DO

  END SELECT

END FUNCTION model_getPhysicalGroups

!----------------------------------------------------------------------------
!                                               getEntitiesForPhysicalGroup
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Get the tags of the model entities making up the physical group of
! dimension dim and tag tag.

FUNCTION model_getEntitiesForPhysicalGroup( obj, dim, tag ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  INTEGER( I4B ), ALLOCATABLE :: ans( : )

  ! define internal variables
  INTEGER( I4B ) :: ii, n

  SELECT CASE( dim )

  CASE( 0 )

    IF( .NOT. ALLOCATED( obj % PhysicalPointUID ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "          model_getEntitiesForPhysicalGroup()")
      CALL Display( "          Physical points are not defined" )
      STOP
    END IF

    n = SIZE( obj % PhysicalPointUID )

    DO ii = 1, n

      IF( obj % PhysicalPointUID( ii ) .EQ. tag ) THEN
        CALL Convert ( From = obj % Point_PhysicalToEntity( ii ) % Ptr, &
          & TO = ans )
        EXIT
      END IF

    END DO

  CASE( 1 )

    IF( .NOT. ALLOCATED( obj % PhysicalCurveUID ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "          model_getEntitiesForPhysicalGroup()")
      CALL Display( "          Physical Curves are not defined" )
      STOP
    END IF

    n = SIZE( obj % PhysicalCurveUID )

    DO ii = 1, n

      IF( obj % PhysicalCurveUID( ii ) .EQ. tag ) THEN
        CALL Convert ( From = obj % Curve_PhysicalToEntity( ii ) % Ptr, &
          & TO = ans )
        EXIT
      END IF

    END DO

  CASE( 2 )

    IF( .NOT. ALLOCATED( obj % PhysicalSurfaceUID ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "          model_getEntitiesForPhysicalGroup()")
      CALL Display( "          Physical Surfaces are not defined" )
      STOP
    END IF

    n = SIZE( obj % PhysicalSurfaceUID )

    DO ii = 1, n

      IF( obj % PhysicalSurfaceUID( ii ) .EQ. tag ) THEN
        CALL Convert ( From = obj % Surface_PhysicalToEntity( ii ) % Ptr, &
          & TO = ans )
        EXIT
      END IF

    END DO

  CASE( 3 )

    IF( .NOT. ALLOCATED( obj % PhysicalVolumeUID ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "          model_getEntitiesForPhysicalGroup()")
      CALL Display( "          Physical Volumes are not defined" )
      STOP
    END IF

    n = SIZE( obj % PhysicalVolumeUID )

    DO ii = 1, n

      IF( obj % PhysicalVolumeUID( ii ) .EQ. tag ) THEN
        CALL Convert ( From = obj % Volume_PhysicalToEntity( ii ) % Ptr, &
          & TO = ans )
        EXIT
      END IF

    END DO

  END SELECT
END FUNCTION model_getEntitiesForPhysicalGroup

!----------------------------------------------------------------------------
!                                               getPhysicalGroupsForEntity
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Get the tags of the physical groups (if any) to which the model entity of
! dimension dim and tag tag belongs.

FUNCTION model_getPhysicalGroupsForEntity( obj, dim, tag ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  INTEGER( I4B ), ALLOCATABLE :: ans( : )

  ! define internal variables
  INTEGER( I4B ) :: ii, n

  SELECT CASE( dim )

  CASE( 0 )

    IF( .NOT. ALLOCATED( obj % geo % Point ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "          model_getPhysicalGroupsForEntity()")
      CALL Display( "          Points are not added in geometry" )
      STOP
    END IF

    n = obj % geo % tPoints

    DO ii = 1, n

      IF( obj % geo % Point( ii ) % ptr % uid .EQ. tag ) THEN
        CALL Convert ( From = obj % Point_EntityToPhysical( ii ) % Ptr, &
          & TO = ans )
        EXIT
      END IF

    END DO

  CASE( 1 )

    IF( .NOT. ALLOCATED( obj % geo % Curve ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "          model_getPhysicalGroupsForEntity()")
      CALL Display( "          Curves are not added in geometry" )
      STOP
    END IF

    n = obj % geo % tCurves

    DO ii = 1, n

      IF( obj % geo % Curve( ii ) % ptr % uid .EQ. tag ) THEN
        CALL Convert ( From = obj % Curve_EntityToPhysical( ii ) % Ptr, &
          & TO = ans )
        EXIT
      END IF

    END DO

  CASE( 2 )

    IF( .NOT. ALLOCATED( obj % geo % Surface ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "          model_getPhysicalGroupsForEntity()")
      CALL Display( "          Surfaces are not added in geometry" )
      STOP
    END IF

    n = obj % geo % tSurfaces

    DO ii = 1, n

      IF( obj % geo % Surface( ii ) % ptr % uid .EQ. tag ) THEN
        CALL Convert ( From = obj % Surface_EntityToPhysical( ii ) % Ptr, &
          & TO = ans )
        EXIT
      END IF

    END DO

  CASE( 3 )

    IF( .NOT. ALLOCATED( obj % geo % Volume ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90")
      CALL Display( "          model_getPhysicalGroupsForEntity()")
      CALL Display( "          Volumes are not added in geometry" )
      STOP
    END IF

    n = obj % geo % tVolumes

    DO ii = 1, n

      IF( obj % geo % Volume( ii ) % ptr % uid .EQ. tag ) THEN
        CALL Convert ( From = obj % Volume_EntityToPhysical( ii ) % Ptr, &
          & TO = ans )
        EXIT
      END IF

    END DO

  END SELECT

END FUNCTION model_getPhysicalGroupsForEntity

!----------------------------------------------------------------------------
!                                                             setPhysicalName
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Set the name of the physical group of dimension dim and tag tag.

FUNCTION model_setPhysicalName( obj, dim, tag, name ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ) :: ans

  ! internal varialbles
  INTEGER( I4B ) :: ii, n
  TYPE( String ), ALLOCATABLE :: s( : )
  TYPE( String ) :: ss
  INTEGER( I4B ), ALLOCATABLE :: tags( : )

  ans = 0

  SELECT CASE( dim )

  CASE( 0 )
    IF( .NOT. ALLOCATED( obj % PhysicalPointName ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_setPhysicalName()" )
      CALL Display( "          Physical Points are not set")
      STOP
    END IF

    n = SIZE( obj % PhysicalPointName )
    DO ii = 1, n
      IF( obj % PhysicalPointUID( ii ) .EQ. tag ) THEN
        obj % PhysicalPointName( ii ) = trim( name )
        CALL Convert(  From = obj % Point_PhysicalToEntity( ii ) % ptr, &
          & To = tags )
        EXIT
      END IF
    END DO

    IF( .NOT. ALLOCATED( tags ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_setPhysicalName()" )
      CALL Display( "          Given dim tag not found")
      STOP
    END IF

    n = SIZE( tags )
    ALLOCATE( s( n ) )

    DO ii = 1, n
      s( ii ) = trim( str( tags( ii ), no_sign=.true. ) )
    END DO

    ss = ss % join( s, sep=", " )

    ss = &
      & "Physical Point( " // &
      & '"'// trim( name ) // '"' // &
      & ", " // &
      & trim( str( tag, no_sign=.true. ) ) // &
      & " ) = { " // &
      & trim( ss ) // &
      & " };"

    DEALLOCATE( s, tags )

    ! CALL Display( "gmsh%model:: adding physical point to buffer")
    CALL APPEND( obj % geo % buffer, ss )

  CASE( 1 )

    IF( .NOT. ALLOCATED( obj % PhysicalCurveName ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_setPhysicalName()" )
      CALL Display( "          Physical Curves are not set")
      STOP
    END IF

    n = SIZE( obj % PhysicalCurveName )

    DO ii = 1, n
      IF( obj % PhysicalCurveUID( ii ) .EQ. tag ) THEN
        obj % PhysicalCurveName( ii ) = trim( name )
        CALL Convert(  From = obj % Curve_PhysicalToEntity( ii ) % ptr, &
          & To = tags )
        EXIT
      END IF
    END DO

    IF( .NOT. ALLOCATED( tags ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_setPhysicalName()" )
      CALL Display( "          Given dim tag not found")
      STOP
    END IF
    n = SIZE( tags )
    ALLOCATE( s( n ) )

    DO ii = 1, n
      s( ii ) = trim( str( tags( ii ), no_sign=.true. ) )
    END DO

    ss = ss % join( s, sep=", " )

    ss = &
      & "Physical Curve( " // &
      & '"'// trim( name ) // '"' // &
      & ", " // &
      & trim( str( tag, no_sign=.true. ) ) // &
      & " ) = { " // &
      & trim( ss ) // &
      & " };"

    DEALLOCATE( s, tags )
    ! CALL Display( "gmsh%model:: adding physical curve to buffer")
    CALL APPEND( obj % geo % buffer, ss )

  CASE( 2 )

    IF( .NOT. ALLOCATED( obj % PhysicalSurfaceName ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_setPhysicalName()" )
      CALL Display( "          Physical Surfaces are not set")
      STOP
    END IF

    n = SIZE( obj % PhysicalSurfaceName )
    DO ii = 1, n
      IF( obj % PhysicalSurfaceUID( ii ) .EQ. tag ) THEN
        obj % PhysicalSurfaceName( ii ) = trim( name )
        CALL Convert(  From = obj % Surface_PhysicalToEntity( ii ) % ptr, &
          & To = tags )
        EXIT
      END IF
    END DO

    IF( .NOT. ALLOCATED( tags ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_setPhysicalName()" )
      CALL Display( "          Given dim tag not found")
      STOP
    END IF

    n = SIZE( tags )
    ALLOCATE( s( n ) )

    DO ii = 1, n
      s( ii ) = trim( str( tags( ii ), no_sign=.true. ) )
    END DO

    ss = ss % join( s, sep=", " )

    ss = &
      & "Physical Surface( " // &
      & '"'// trim( name ) // '"' // &
      & ", " // &
      & trim( str( tag, no_sign=.true. ) ) // &
      & " ) = { " // &
      & trim( ss ) // &
      & " };"

    DEALLOCATE( s, tags )
    ! CALL Display( "gmsh%model:: adding physical surface to buffer")
    CALL APPEND( obj % geo % buffer, ss )

  CASE( 3 )

    IF( .NOT. ALLOCATED( obj % PhysicalVolumeName ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_setPhysicalName()" )
      CALL Display( "          Physical Volumes are not set")
      STOP
    END IF

    n = SIZE( obj % PhysicalVolumeName )
    DO ii = 1, n
      IF( obj % PhysicalVolumeUID( ii ) .EQ. tag ) THEN
        obj % PhysicalVolumeName( ii ) = trim( name )
        CALL Convert(  From = obj % Volume_PhysicalToEntity( ii ) % ptr, &
          & To = tags )
        EXIT
      END IF
    END DO

    IF( .NOT. ALLOCATED( tags ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_setPhysicalName()" )
      CALL Display( "          Given dim tag not found")
      STOP
    END IF
    n = SIZE( tags )
    ALLOCATE( s( n ) )

    DO ii = 1, n
      s( ii ) = trim( str( tags( ii ), no_sign=.true. ) )
    END DO

    ss = ss % join( s, sep=", " )

    ss = &
      & "Physical Volume( " // &
      & '"'// trim( name ) // '"' // &
      & ", " // &
      & trim( str( tag, no_sign=.true. ) ) // &
      & " ) = { " // &
      & trim( ss ) // &
      & " };"

    DEALLOCATE( s, tags )
    ! WRITE( obj % buffer % unitno, "(DT)" ) ss

    ! CALL Display( "gmsh%model:: adding physical volume to buffer")
    CALL APPEND( obj % geo % buffer, ss )

  END SELECT

END FUNCTION model_setPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION model_getPhysicalName( obj, dim, tag ) RESULT( ans )
  CLASS( gmshModel_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: dim, tag
  TYPE( String ) :: ans

  INTEGER( I4B ) :: ii, n

  SELECT CASE( dim )

  CASE( 0 )

    IF( .NOT. ALLOCATED( obj % PhysicalPointName ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_getPhysicalName()" )
      CALL Display( "          Physical Points are not set")
      STOP
    END IF

    n = SIZE( obj % PhysicalPointName )
    DO ii = 1, n
      IF( obj % PhysicalPointUID( ii ) .EQ. tag ) THEN
        ans = obj % PhysicalPointName( ii )
        EXIT
      END IF
    END DO

  CASE( 1 )

    IF( .NOT. ALLOCATED( obj % PhysicalCurveName ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_getPhysicalName()" )
      CALL Display( "          Physical Curves are not set")
      STOP
    END IF

    n = SIZE( obj % PhysicalCurveName )
    DO ii = 1, n
      IF( obj % PhysicalCurveUID( ii ) .EQ. tag ) THEN
        ans = obj % PhysicalCurveName( ii )
        EXIT
      END IF
    END DO

  CASE( 2 )

    IF( .NOT. ALLOCATED( obj % PhysicalSurfaceName ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_getPhysicalName()" )
      CALL Display( "          Physical Surfaces are not set")
      STOP
    END IF

    n = SIZE( obj % PhysicalSurfaceName )
    DO ii = 1, n
      IF( obj % PhysicalSurfaceUID( ii ) .EQ. tag ) THEN
        ans = obj % PhysicalSurfaceName( ii )
        EXIT
      END IF
    END DO

  CASE( 3 )

    IF( .NOT. ALLOCATED( obj % PhysicalVolumeName ) ) THEN
      CALL Display( "ERROR:: gmshModel_Class.f90" )
      CALL Display( "          model_getPhysicalName()" )
      CALL Display( "          Physical Volumes are not set")
      STOP
    END IF

    n = SIZE( obj % PhysicalVolumeName )
    DO ii = 1, n
      IF( obj % PhysicalVolumeUID( ii ) .EQ. tag ) THEN
        ans = obj % PhysicalVolumeName( ii )
        EXIT
      END IF
    END DO

  END SELECT
END FUNCTION model_getPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE gmshModel_Class

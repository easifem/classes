! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE( Domain_Class ) PhysicalNamesMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getTotalPhysicalEntities
  INTEGER( I4B ) :: ii

  IF( .NOT. PRESENT( dim ) ) THEN
    IF( ALLOCATED( obj%NSDVec ) ) THEN
      ans = SIZE( obj%NSDVec )
    ELSE
      ans = 0
    END IF
  ELSE
    ans = 0
    DO ii = 1, SIZE( dim )
      SELECT CASE( dim(ii) )
      CASE( 0 )
        ans = ans + getTotalPhysicalPoints( obj )
      CASE( 1 )
        ans = ans + getTotalPhysicalCurves( obj )
      CASE( 2 )
        ans = ans + getTotalPhysicalSurfaces( obj )
      CASE( 3 )
        ans = ans + getTotalPhysicalVolumes( obj )
      END SELECT
    END DO
  END IF
END PROCEDURE Domain_getTotalPhysicalEntities

!----------------------------------------------------------------------------
!                                                    getTotalPhysicalPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE getTotalPhysicalPoints
  IF( ALLOCATED( obj%NSDVec ) ) THEN
    ans = COUNT( obj%NSDVec .EQ. 0 )
  ELSE
    ans = 0
  END IF
END PROCEDURE getTotalPhysicalPoints

!----------------------------------------------------------------------------
!                                                     getTotalPhysicalCurves
!----------------------------------------------------------------------------

MODULE PROCEDURE getTotalPhysicalCurves
  IF( ALLOCATED( obj%NSDVec ) ) THEN
    ans = COUNT( obj%NSDVec .EQ. 1 )
  ELSE
    ans = 0
  END IF
END PROCEDURE getTotalPhysicalCurves

!----------------------------------------------------------------------------
!                                                  getTotalPhysicalSurfaces
!----------------------------------------------------------------------------

MODULE PROCEDURE getTotalPhysicalSurfaces
  IF( ALLOCATED( obj%NSDVec ) ) THEN
    ans = COUNT( obj%NSDVec .EQ. 2 )
  ELSE
    ans = 0
  END IF
END PROCEDURE getTotalPhysicalSurfaces

!----------------------------------------------------------------------------
!                                                    getTotalPhysicalVolumes
!----------------------------------------------------------------------------

MODULE PROCEDURE getTotalPhysicalVolumes
  IF( ALLOCATED( obj%NSDVec ) ) THEN
    ans = COUNT( obj%NSDVec .EQ. 3 )
  ELSE
    ans = 0
  END IF
END PROCEDURE getTotalPhysicalVolumes

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getIndex_a
  ! Define internal variables
  INTEGER( I4B ) :: j

  ans = 0
  IF( ALLOCATED( obj%NSDVec ) ) THEN
    DO j = 1, SIZE( obj%NSDVec )
      IF( obj%PhysicalName( j ) .EQ. Name ) THEN
        ans = j
        EXIT
      END IF
    END DO
  END IF

END PROCEDURE Domain_getIndex_a

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getIndex_b
  ! Define internal variables
  INTEGER( I4B ) :: i

  ans = 0
  IF( ALLOCATED( obj%NSDVec ) ) THEN
    DO i = 1, SIZE( Name )
      ans( i ) = Domain_getIndex_a( obj, Name(i) )
    END DO
  END IF
END PROCEDURE Domain_getIndex_b

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getIndex_c
  ! Define internal variables
  INTEGER( I4B ) :: k

  ans = 0
  DO k = 1, SIZE( obj%NSDVec )
    IF( obj%NSDVec( k ) .EQ. dim .AND. obj%Tag( k ) .EQ. Tag ) THEN
      ans = k
    END IF
  END DO
END PROCEDURE Domain_getIndex_c

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getIndex_d
  ! Define internal variables
  INTEGER( I4B ) :: tPoints, k, i

  tPoints = obj%getTotalPhysicalEntities( [dim] )

  IF( tPoints .NE. 0 ) THEN
    ALLOCATE( ans( tPoints ) )
    i = 0
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. dim ) THEN
        i = i + 1
        ans( i ) = k
      END IF
    END DO
  ELSE
    ALLOCATE( ans(0) )
  END IF
END PROCEDURE Domain_getIndex_d

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getIndex_e
  INTEGER( I4B ) :: i
  ans = 0_I4B
  DO i = 1, SIZE( tag )
    ans( i ) = Domain_getIndex_c( obj=obj, dim = dim, tag=tag(i) )
  END DO
END PROCEDURE Domain_getIndex_e

!----------------------------------------------------------------------------
!                                                          getPhysicalNames
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getPhysicalNames
  INTEGER( I4B ) :: ii
  IF( PRESENT( dim ) ) THEN
    SELECT CASE( dim )
    CASE( 0 )
      ans = getPhysicalPointNames(obj)
    CASE( 1 )
      ans = getPhysicalCurveNames(obj)
    CASE( 2 )
      ans = getPhysicalSurfaceNames(obj)
    CASE( 3 )
      ans = getPhysicalVolumeNames(obj)
    END SELECT
  ELSE
    ALLOCATE( ans( SIZE( obj%physicalName ) ) )
    DO ii = 1, SIZE( obj%physicalName )
      ans( ii ) = TRIM( obj%physicalName( ii ) )
    END DO
  END IF
END PROCEDURE Domain_getPhysicalNames

!----------------------------------------------------------------------------
!                                                          PhysicalPointNames
!----------------------------------------------------------------------------

MODULE PROCEDURE getPhysicalPointNames
  ! Define internal variables
  INTEGER( I4B ) :: tPoints, i, k

  tPoints = getTotalPhysicalPoints( obj )
  ALLOCATE( ans( tPoints ) )
  IF( tPoints .NE. 0 ) THEN
    i = 0;
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. 0 ) THEN
        i = i + 1
        ans( i ) = TRIM(obj%PhysicalName( k ))
      END IF
    END DO
  END IF
END PROCEDURE getPhysicalPointNames

!----------------------------------------------------------------------------
!                                                          PhysicalCurveNames
!----------------------------------------------------------------------------

MODULE PROCEDURE getPhysicalCurveNames
  ! Define internal variables
  INTEGER( I4B ) :: tLines, i, k

  tLines = getTotalPhysicalCurves(obj)
  ALLOCATE( ans( tLines ) )
  IF( tLines .NE. 0 ) THEN
    i = 0;
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. 1 ) THEN
        i = i + 1
        ans( i ) = TRIM(obj%PhysicalName( k ))
      END IF
    END DO
  END IF
END PROCEDURE getPhysicalCurveNames

!----------------------------------------------------------------------------
!                                                       PhysicalSurfaceNames
!----------------------------------------------------------------------------

MODULE PROCEDURE getPhysicalSurfaceNames
  ! Define internal variables
  INTEGER( I4B ) :: tSurfaces, i, k

  tSurfaces = getTotalPhysicalSurfaces(obj)
  ALLOCATE( ans( tSurfaces ) )
  IF( tSurfaces .NE. 0 ) THEN
    i = 0;
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. 2 ) THEN
        i = i + 1
        ans( i ) = TRIM(obj%PhysicalName( k ))
      END IF
    END DO
  END IF
END PROCEDURE getPhysicalSurfaceNames

!----------------------------------------------------------------------------
!                                                       PhysicalVolumeNames
!----------------------------------------------------------------------------

MODULE PROCEDURE getPhysicalVolumeNames
  ! Define internal variables
  INTEGER( I4B ) :: tVolumes, i, k

  tVolumes = getTotalPhysicalVolumes(obj)
  ALLOCATE( ans( tVolumes ) )

  IF( tVolumes .NE. 0 ) THEN
    i = 0;
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. 3 ) THEN
        i = i + 1
        ans( i ) = TRIM(obj%PhysicalName( k ))
      END IF
    END DO
  END IF
END PROCEDURE getPhysicalVolumeNames

!----------------------------------------------------------------------------
!                                                            getPhysicalTags
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getPhysicalTags
  IF( PRESENT( dim ) ) THEN
    SELECT CASE( dim )
    CASE( 0 )
      ans = getPhysicalPointTags( obj )
    CASE( 1 )
      ans = getPhysicalCurveTags( obj )
    CASE( 2 )
      ans = getPhysicalSurfaceTags( obj )
    CASE( 3 )
      ans = getPhysicalVolumeTags( obj )
    END SELECT
  ELSE
    ans = obj%Tag
  END IF
END PROCEDURE Domain_getPhysicalTags

!----------------------------------------------------------------------------
!                                                      getPhysicalPointTags
!----------------------------------------------------------------------------

MODULE PROCEDURE getPhysicalPointTags
  ! Define internal variables
  INTEGER( I4B ) :: tSize, i, k
  INTEGER( I4B ), PARAMETER :: dim = 0

  tSize = getTotalPhysicalPoints( obj )
  ALLOCATE( ans( tSize ) )

  IF( tSize .NE. 0 ) THEN
    i = 0;
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. dim ) THEN
        i = i + 1
        ans( i ) = obj%Tag( k )
      END IF
    END DO
  END IF
END PROCEDURE getPhysicalPointTags

!----------------------------------------------------------------------------
!                                                       getPhysicalCurveTag
!----------------------------------------------------------------------------

MODULE PROCEDURE getPhysicalCurveTags
  ! Define internal variables
  INTEGER( I4B ) :: tSize, i, k
  INTEGER( I4B ), PARAMETER :: dim = 1

  tSize = getTotalPhysicalCurves( obj )
  ALLOCATE( ans( tSize ) )

  IF( tSize .NE. 0 ) THEN
    i = 0;
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. dim ) THEN
        i = i + 1
        ans( i ) = obj%Tag( k )
      END IF
    END DO
  END IF
END PROCEDURE getPhysicalCurveTags

!----------------------------------------------------------------------------
!                                                      getPhysicalSurfaceTag
!----------------------------------------------------------------------------

MODULE PROCEDURE getPhysicalSurfaceTags
  ! Define internal variables
  INTEGER( I4B ) :: tSize, i, k
  INTEGER( I4B ), PARAMETER :: dim = 2

  tSize = getTotalPhysicalSurfaces( obj )
  ALLOCATE( ans( tSize ) )

  IF( tSize .NE. 0 ) THEN
    i = 0;
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. dim ) THEN
        i = i + 1
        ans( i ) = obj%Tag( k )
      END IF
    END DO
  END IF
END PROCEDURE getPhysicalSurfaceTags

!----------------------------------------------------------------------------
!                                                       getPhysicalVolumeTag
!----------------------------------------------------------------------------

MODULE PROCEDURE getPhysicalVolumeTags
  ! Define internal variables
  INTEGER( I4B ) :: tSize, i, k
  INTEGER( I4B ), PARAMETER :: dim = 3

  tSize = getTotalPhysicalVolumes( obj )
  ALLOCATE( ans( tSize ) )

  IF( tSize .NE. 0 ) THEN
    i = 0;
    DO k = 1, SIZE( obj%NSDVec )
      IF( obj%NSDVec( k ) .EQ. dim ) THEN
        i = i + 1
        ans( i ) = obj%Tag( k )
      END IF
    END DO
  END IF
END PROCEDURE getPhysicalVolumeTags

!----------------------------------------------------------------------------
!                                                                 WhoAmI
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_WhoAmI
  ! Define internal variables
  INTEGER( I4B ) :: NSD
  NSD = obj%NSDVec( I )
  SELECT CASE( NSD )
  CASE( 0 )
    ans = "PhysicalPoint"
  CASE( 1 )
    ans = "PhysicalCurve"
  CASE( 2 )
    ans = "PhysicalSurface"
  CASE( 3 )
    ans = "PhysicalVolume"
  END SELECT
END PROCEDURE Domain_WhoAmI

!----------------------------------------------------------------------------
!                                                            AppendEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_AppendEntities
  CALL APPEND( obj%Entities( indx ), EntityTag )
END PROCEDURE Domain_AppendEntities

!----------------------------------------------------------------------------
!                                                           IncNumElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_IncNumElements
  INTEGER( I4B ) :: incr0

  incr0 = input( option = incr, default = 1 )
  obj%numElements( indx ) = obj%numElements( indx ) + incr0
END PROCEDURE Domain_IncNumElements

!----------------------------------------------------------------------------
!                                                               getEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getEntities
  Ans = get( obj%Entities( indx ), TypeIntI4B )
END PROCEDURE Domain_getEntities

!----------------------------------------------------------------------------
!                                                           IncNumNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_IncNumNodes
  INTEGER( I4B ) :: incr0

  incr0 = input( option = incr, default = 1 )
  obj%numNodes( indx ) = obj%numNodes( indx ) + incr0
END PROCEDURE Domain_IncNumNodes

!----------------------------------------------------------------------------
!                                                                 getNSD
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNSD
  ans = obj%NSDVec
END PROCEDURE Domain_getNSD

!----------------------------------------------------------------------------
!                                                            getNumElements
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNumElements
  ans = obj%numElements
END PROCEDURE Domain_getNumElements

!----------------------------------------------------------------------------
!                                                               getNumNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_getNumNodes
  ans = obj%numNodes
END PROCEDURE Domain_getNumNodes

!----------------------------------------------------------------------------
!                                                               setNumNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setNumNodes
  obj%numNodes( indx ) = numNode
END PROCEDURE Domain_setNumNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE PhysicalNamesMethods
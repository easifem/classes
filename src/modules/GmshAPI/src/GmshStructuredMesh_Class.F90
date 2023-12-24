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

MODULE GmshStructuredMesh_Class
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE FPL_Method
USE Gmsh_Class
USE BaseMethod
IMPLICIT NONE
PRIVATE
PUBLIC :: GmshStructuredMesh_
PUBLIC :: GmshStructuredMeshPointer_
PUBLIC :: SetGmshStructuredMeshParam
INTEGER(I4B), PARAMETER :: Progression = 1
INTEGER(I4B), PARAMETER :: Bump = 2
INTEGER(I4B), PUBLIC, PARAMETER :: GMSH_API_PROGRESSION = 1
INTEGER(I4B), PUBLIC, PARAMETER :: GMSH_API_BUMP = 2
CHARACTER(*), PARAMETER :: modName = "StructuredMesh_Class"
CHARACTER(*), PARAMETER :: myprefix = "StructuredMesh"
CHARACTER(*), PARAMETER :: MeshTypeName(2) = ["Progression", "Bump       "]

INTERFACE SetGmshStructuredMeshParam
  MODULE PROCEDURE SetGmshStructuredMeshParam1
  MODULE PROCEDURE SetGmshStructuredMeshParam2
END INTERFACE SetGmshStructuredMeshParam

!----------------------------------------------------------------------------
!                                                      GmshStructuredMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  The data type to create structured mesh by using Gmsh

TYPE :: GmshStructuredMesh_
  ! PRIVATE
  LOGICAL(LGT) :: recombineAll = .TRUE.
  !! All surfaces will be recombine into quad or hexahedron
  INTEGER(I4B) :: nsd = 2_I4B
  !! Spatial dimension
  TYPE(String) :: filename
  !! Name of the mesh file to be generated
  TYPE(RealMatrix_) :: points(3)
  !! points on axis 1, axis 2, axis 3
  !! points(1) are points on axis 1
  !! points(2) are points on axis 2
  !! points(3) are points on axis 3
  INTEGER(I4B) :: tPoints(3) = 0
  !! Total points on axis 1 to define the region
  !! Total points on axis 2 to define the region
  !! Total points on axis 3 to define the region
  REAL(DFP), ALLOCATABLE :: allPoints(:, :)
  !! All the points
  TYPE(IntVector_) :: transfinitePoints(3)
  !! transfinitePoints on lines
  !! transfinitePoints(1) is transfinitePoints on axis 1
  !! transfinitePoints(2) is transfinitePoints on axis 2
  !! transfinitePoints(3) is transfinitePoints on axis 3
  INTEGER(I4B), ALLOCATABLE :: edge_tfp(:)
  !! transfinitePoints for transfinite curves
  REAL(DFP), ALLOCATABLE :: edge_coef(:)
  !! coef ofr transfinite curves
  TYPE(String), ALLOCATABLE :: edge_meshType(:)
  !! mesh type for transfinite curves
  INTEGER(I4B), ALLOCATABLE :: edges(:, :)
  !! The edges
  INTEGER(I4B) :: tEdges1 = 0
  !! Total edges parallel to axis 1
  INTEGER(I4B) :: tEdges2 = 0
  !! Total edges parallel to axis 2
  INTEGER(I4B) :: tEdges3 = 0
  !! Total edges parallel to axis 3
  INTEGER(I4B) :: tEdges = 0
  !! Total number of edges = tEdges1+tEdges2+tEdges3
  INTEGER(I4B), ALLOCATABLE :: curveLoops(:, :)
  !! Surface loops for boxes, it means boxes in terms of
  !! edges
  INTEGER(I4B) :: tSurfacesXY = 0
  !! Total number of surfaces parallen to XY plane
  INTEGER(I4B) :: tSurfacesYZ = 0
  !! Total number of surfaces parallen to YZ plane
  INTEGER(I4B) :: tSurfacesXZ = 0
  !! Total number of surfaces parallen to XZ plane
  INTEGER(I4B) :: tSurfaces = 0
  !! Total number of surfaces
  !! If there are no holes then tSurfaces equal to tVolumes
  INTEGER(I4B) :: tVolumes = 0
  !! Total number of boxes
  INTEGER(I4B), ALLOCATABLE :: surfaceLoops(:, :)
  !! Surface loops for making volumes
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & mesh_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => mesh_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => mesh_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Generate => mesh_Generate
  PROCEDURE, PUBLIC, PASS(obj) :: GeneratePoints => mesh_GeneratePoints
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateCurves => mesh_GenerateCurves
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateSurfaces => mesh_GenerateSurfaces
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateVolumes => mesh_GenerateVolumes
  PROCEDURE, PUBLIC, PASS(obj) :: Display => mesh_Display
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeNumber => mesh_GetNodeNumber
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeNumberOnAxis1 =>  &
    & mesh_GetEdgeNumberOnAxis1
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeNumberOnAxis2 =>  &
    & mesh_GetEdgeNumberOnAxis2
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeNumberOnAxis3 =>  &
    & mesh_GetEdgeNumberOnAxis3
  PROCEDURE, PUBLIC, PASS(obj) :: GetSurfaceNumberXY =>  &
    & mesh_GetSurfaceNumberXY
  PROCEDURE, PUBLIC, PASS(obj) :: GetSurfaceNumberYZ =>  &
    & mesh_GetSurfaceNumberYZ
  PROCEDURE, PUBLIC, PASS(obj) :: GetSurfaceNumberXZ =>  &
    & mesh_GetSurfaceNumberXZ
END TYPE GmshStructuredMesh_

!----------------------------------------------------------------------------
!                                             GmshStructuredMeshPointer_
!----------------------------------------------------------------------------

TYPE :: GmshStructuredMeshPointer_
  CLASS(GmshStructuredMesh_), POINTER :: ptr => NULL()
END TYPE GmshStructuredMeshPointer_

!----------------------------------------------------------------------------
!                                                                Contains
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                              SetGmshStructuredMeshParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Set parameter

SUBROUTINE SetGmshStructuredMeshParam1( &
  & param,  &
  & filename, &
  & pointsOnAxis1,  &
  & transfinitePointsOnAxis1,  &
  & pointsOnAxis2,  &
  & transfinitePointsOnAxis2,  &
  & pointsOnAxis3,  &
  & transfinitePointsOnAxis3,  &
  & recombineAll,  &
  & meshTypeOnAxis1,  &
  & meshTypeOnAxis2,  &
  & meshTypeOnAxis3,  &
  & coefOnAxis1,  &
  & coefOnAxis2,  &
  & coefOnAxis3)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  !! spatial dimension
  CHARACTER(*), INTENT(IN) :: filename
  !! name of the mesh file to be generated
  REAL(DFP), INTENT(IN) :: pointsOnAxis1(:, :)
  !! points on axis 1
  INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis1(:)
  !! transfinitePoints on axis 1
  REAL(DFP), INTENT(IN) :: pointsOnAxis2(:, :)
  !! points on axis 2
  INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis2(:)
  !! transfinitePoints on axis 2
  REAL(DFP), OPTIONAL, INTENT(IN) :: pointsOnAxis3(:, :)
  !! points on axis 3
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: transfinitePointsOnAxis3(:)
  !! transfinitePoints on axis 3
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombineAll
  !! If true we combine triangle and tetrahedron into quad and hexahedron
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis1(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis2(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis3(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis1(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis2(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis3(:)

  ! internal variables
  INTEGER(I4B) :: tPoints(3), tVolumes, aint, bint, nsd, ii
  REAL(DFP), ALLOCATABLE :: pointsOnAxis3_(:, :)
  REAL(DFP), ALLOCATABLE :: coefOnAxis1_(:)
  REAL(DFP), ALLOCATABLE :: coefOnAxis2_(:)
  REAL(DFP), ALLOCATABLE :: coefOnAxis3_(:)
  INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis1_(:)
  INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis2_(:)
  INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis3_(:)
  INTEGER(I4B), ALLOCATABLE :: transfinitePointsOnAxis3_(:)
  REAL(DFP), PARAMETER :: r2type(1, 1) = 0, r1type(1) = 0.0_DFP
  INTEGER(I4B), PARAMETER :: i1type(1) = 0
  CHARACTER(*), PARAMETER :: myName = "SetGmshStructuredMeshParam1()"
  LOGICAL(LGT) :: recombineAll_, is3present

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  aint = SIZE(pointsOnAxis1, 1)
  IF (aint .NE. 3_I4B) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: The number of rows in pointsOnAxis1 should be 3'//  &
    & ' but it is '//tostring(aint))
  END IF

  aint = SIZE(pointsOnAxis1, 2)
  bint = SIZE(transfinitePointsOnAxis1)
  IF (aint .NE. bint + 1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: SIZE(pointsOnAxis1, 2) should be same as '//  &
      & ' number of transfinitePointsOnAxis1 + 1'//  &
      & ' but '//tostring(aint)//"!="//tostring(bint + 1))
  END IF

  aint = SIZE(pointsOnAxis2, 1)
  IF (aint .NE. 3_I4B) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: size(pointsOnAxis2, 1) should be 3'//  &
      & ' but it is '//tostring(aint))
  END IF

  aint = SIZE(pointsOnAxis2, 2)
  bint = SIZE(transfinitePointsOnAxis2)
  IF (aint .NE. bint + 1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: size(pointsOnAxis2, 2) should be same as '//  &
      & ' number of transfinitePointsOnAxis2 + 1'//  &
      & ' but '//tostring(aint)//"!="//tostring(bint + 1))
  END IF

  tPoints = 1
  tPoints(1) = SIZE(pointsOnAxis1, 2)
  tPoints(2) = SIZE(pointsOnAxis2, 2)

  is3present = PRESENT(pointsOnAxis3)
  IF (is3present) THEN
    nsd = 3
    aint = SIZE(pointsOnAxis3, 1)
    IF (aint .NE. 3_I4B) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis3, 1) should be 3'//  &
        & ' but it is '//tostring(aint))
    END IF

    IF (.NOT. PRESENT(transfinitePointsOnAxis3)) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: transfinitePointsOnAxis3 should be present '// &
        & 'when pointsOnAxis3 are present.')
    END IF

    aint = SIZE(pointsOnAxis3, 2)
    bint = SIZE(transfinitePointsOnAxis3)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis3, 2) should be same as '//  &
        & ' number of transfinitePointsOnAxis3 + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF

    tPoints(3) = SIZE(pointsOnAxis3, 2)
    pointsOnAxis3_ = pointsOnAxis3
    transfinitePointsOnAxis3_ = transfinitePointsOnAxis3
  ELSE
    nsd = 2
    aint = SIZE(pointsOnAxis1, 1)
    CALL Reallocate(pointsOnAxis3_, aint, 1_I4B)
    CALL Reallocate(transfinitePointsOnAxis3_, 1_I4B)
    pointsOnAxis3_(1:aint, 1) = pointsOnAxis1(1:aint, 1)
    transfinitePointsOnAxis3_(1) = 1
    tPoints(3) = 1
  END IF

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[1]')

  IF (PRESENT(meshTypeOnAxis1)) THEN
    aint = SIZE(pointsOnAxis1, 2)
    bint = SIZE(meshTypeOnAxis1)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis1, 2) should be same as '//  &
        & ' number of size(meshTypeOnAxis1) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    meshTypeOnAxis1_ = meshTypeOnAxis1
  ELSE
    CALL Reallocate(meshTypeOnAxis1_, tPoints(1) - 1)
    meshTypeOnAxis1_ = Progression
  END IF

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[2]')

  IF (PRESENT(meshTypeOnAxis2)) THEN
    aint = SIZE(pointsOnAxis2, 2)
    bint = SIZE(meshTypeOnAxis2)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis2, 2) should be same as '//  &
        & ' number of size(meshTypeOnAxis2) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    meshTypeOnAxis2_ = meshTypeOnAxis2
  ELSE
    CALL Reallocate(meshTypeOnAxis2_, tPoints(2) - 1)
    meshTypeOnAxis2_ = Progression
  END IF

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[3]')

  IF (PRESENT(meshTypeOnAxis3)) THEN
    aint = SIZE(pointsOnAxis3, 2)
    bint = SIZE(meshTypeOnAxis3)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis3, 2) should be same as '//  &
        & ' number of size(meshTypeOnAxis3) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    meshTypeOnAxis3_ = meshTypeOnAxis3
  ELSE
    CALL Reallocate(meshTypeOnAxis3_, MAX(tPoints(3) - 1, 1))
    DO ii = 1, SIZE(meshTypeOnAxis3_)
      meshTypeOnAxis3_(ii) = Progression
    END DO
  END IF

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[4]')

  IF (PRESENT(coefOnAxis1)) THEN
    aint = SIZE(pointsOnAxis1, 2)
    bint = SIZE(coefOnAxis1)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis1, 2) should be same as '//  &
        & ' number of size(coefOnAxis1) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    coefOnAxis1_ = coefOnAxis1
  ELSE
    CALL Reallocate(coefOnAxis1_, tPoints(1) - 1)
    coefOnAxis1_ = 1.0_DFP
  END IF

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[5]')

  IF (PRESENT(coefOnAxis2)) THEN
    aint = SIZE(pointsOnAxis2, 2)
    bint = SIZE(coefOnAxis2)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis2, 2) should be same as '//  &
        & ' number of size(coefOnAxis2) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    coefOnAxis2_ = coefOnAxis2
  ELSE
    CALL Reallocate(coefOnAxis2_, tPoints(2) - 1)
    coefOnAxis2_ = 1.0_DFP
  END IF

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[6]')

  IF (PRESENT(coefOnAxis3)) THEN
    aint = SIZE(pointsOnAxis3, 2)
    bint = SIZE(coefOnAxis3)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis3, 2) should be same as '//  &
        & ' number of size(coefOnAxis3) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    coefOnAxis3_ = coefOnAxis3
  ELSE
    CALL Reallocate(coefOnAxis3_, MAX(tPoints(3) - 1, 1))
    coefOnAxis3_ = 1.0_DFP
  END IF

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[7]')

  recombineAll_ = input(option=recombineAll, default=.TRUE.)
  CALL Set(obj=param, datatype=.TRUE., prefix=myprefix, key="recombineAll",  &
    & VALUE=recombineAll_)

  CALL Set(obj=param, datatype="char", prefix=myprefix, key="filename",  &
    & VALUE=filename)

  CALL Set(obj=param, datatype=TypeIntI4B, prefix=myprefix, key="nsd",  &
    & VALUE=nsd)

  tVolumes = MAX(tPoints(1) - 1, 1_I4B) * MAX(tPoints(2) - 1, 1_I4B) *   &
    & MAX(tPoints(3) - 1, 1_I4B)

  CALL Set(obj=param, datatype=1_I4B, prefix=myprefix, key="tVolumes",  &
    & VALUE=tVolumes)

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[8]')

  CALL Set(obj=param, datatype=[1_I4B], prefix=myprefix, key="tPoints",  &
    & VALUE=tPoints)

  CALL Set(obj=param, datatype=r2type, prefix=myprefix, &
    &  key="pointsOnAxis1", VALUE=pointsOnAxis1)

  CALL Set(obj=param, datatype=r2type, prefix=myprefix, &
    &  key="pointsOnAxis2", VALUE=pointsOnAxis2)

  CALL Set(obj=param, datatype=r2type, prefix=myprefix, &
    &  key="pointsOnAxis3", VALUE=pointsOnAxis3_)

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[9]')

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="transfinitePointsOnAxis1", VALUE=transfinitePointsOnAxis1)

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="transfinitePointsOnAxis2", VALUE=transfinitePointsOnAxis2)

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="transfinitePointsOnAxis3", VALUE=transfinitePointsOnAxis3_)

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[10]')

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="meshTypeOnAxis1", VALUE=meshTypeOnAxis1_)

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[10.1]')
  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="meshTypeOnAxis2", VALUE=meshTypeOnAxis2_)

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[10.2]')
  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="meshTypeOnAxis3", VALUE=meshTypeOnAxis3_)

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[11]')

  CALL Set(obj=param, datatype=r1type, prefix=myprefix, &
    &  key="coefOnAxis1", VALUE=coefOnAxis1_)

  CALL Set(obj=param, datatype=r1type, prefix=myprefix, &
    &  key="coefOnAxis2", VALUE=coefOnAxis2_)

  CALL Set(obj=param, datatype=r1type, prefix=myprefix, &
    &  key="coefOnAxis3", VALUE=coefOnAxis3_)

  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    & '[12]')

  IF (ALLOCATED(pointsOnAxis3_)) DEALLOCATE (pointsOnAxis3_)
  IF (ALLOCATED(coefOnAxis1_)) DEALLOCATE (coefOnAxis1_)
  IF (ALLOCATED(coefOnAxis2_)) DEALLOCATE (coefOnAxis2_)
  IF (ALLOCATED(coefOnAxis3_)) DEALLOCATE (coefOnAxis3_)

  IF (ALLOCATED(meshTypeOnAxis1_)) DEALLOCATE (meshTypeOnAxis1_)
  IF (ALLOCATED(meshTypeOnAxis2_)) DEALLOCATE (meshTypeOnAxis2_)
  IF (ALLOCATED(meshTypeOnAxis3_)) DEALLOCATE (meshTypeOnAxis3_)

  IF (ALLOCATED(transfinitePointsOnAxis3_))  &
    & DEALLOCATE (transfinitePointsOnAxis3_)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetGmshStructuredMeshParam1

!----------------------------------------------------------------------------
!                                               SetGmshStructuredMeshParam
!----------------------------------------------------------------------------

SUBROUTINE SetGmshStructuredMeshParam2( &
  & param, filename, pointsOnAxis1, transfinitePointsOnAxis1,  &
  & pointsOnAxis2, transfinitePointsOnAxis2, pointsOnAxis3,  &
  & transfinitePointsOnAxis3, recombineAll, meshTypeOnAxis1,  &
  & meshTypeOnAxis2, meshTypeOnAxis3, coefOnAxis1, coefOnAxis2,  &
  & coefOnAxis3)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  !! spatial dimension
  CHARACTER(*), INTENT(IN) :: filename
  !! name of the mesh file to be generated
  REAL(DFP), INTENT(IN) :: pointsOnAxis1(:)
  !! points on axis 1
  INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis1(:)
  !! transfinitePoints on axis 1
  REAL(DFP), INTENT(IN) :: pointsOnAxis2(:)
  !! points on axis 2
  INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis2(:)
  !! transfinitePoints on axis 2
  REAL(DFP), OPTIONAL, INTENT(IN) :: pointsOnAxis3(:)
  !! points on axis 3
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: transfinitePointsOnAxis3(:)
  !! transfinitePoints on axis 3
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombineAll
  !! If true we combine triangle and tetrahedron into quad and hexahedron
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis1(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis2(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis3(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis1(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis2(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis3(:)

  CHARACTER(*), PARAMETER :: myName = "SetGmshStructuredMeshParam2()"
  LOGICAL(LGT) :: is3present
  INTEGER(I4B) :: tsize
  REAL(DFP), ALLOCATABLE :: p1(:, :), p2(:, :), p3(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  is3present = PRESENT(pointsOnAxis3)

  tsize = SIZE(pointsOnAxis1)
  CALL Reallocate(p1, 3, tsize)
  p1(1, :) = pointsOnAxis1

  tsize = SIZE(pointsOnAxis2)
  CALL Reallocate(p2, 3, tsize)
  p2(2, :) = pointsOnAxis2

  IF (is3present) THEN
    tsize = SIZE(pointsOnAxis3)
    CALL Reallocate(p3, 3, tsize)
    p3(3, :) = pointsOnAxis3
  END IF

  IF (is3present) THEN
    CALL SetGmshStructuredMeshParam1(param=param, filename=filename,  &
      & pointsOnAxis1=p1, pointsOnAxis2=p2, pointsOnAxis3=p3, &
      & transfinitePointsOnAxis1=transfinitePointsOnAxis1,  &
      & transfinitePointsOnAxis2=transfinitePointsOnAxis2,  &
      & transfinitePointsOnAxis3=transfinitePointsOnAxis3,  &
      & recombineAll=recombineAll,  &
      & meshTypeOnAxis1=meshTypeOnAxis1,  &
      & meshTypeOnAxis2=meshTypeOnAxis2,  &
      & meshTypeOnAxis3=meshTypeOnAxis3,  &
      & coefOnAxis1=coefOnAxis1,  &
      & coefOnAxis2=coefOnAxis2,  &
      & coefOnAxis3=coefOnAxis3)

  ELSE
    CALL SetGmshStructuredMeshParam1(param=param, filename=filename,  &
      & pointsOnAxis1=p1, pointsOnAxis2=p2,  &
      & transfinitePointsOnAxis1=transfinitePointsOnAxis1,  &
      & transfinitePointsOnAxis2=transfinitePointsOnAxis2,  &
      & recombineAll=recombineAll,  &
      & meshTypeOnAxis1=meshTypeOnAxis1,  &
      & meshTypeOnAxis2=meshTypeOnAxis2,  &
      & coefOnAxis1=coefOnAxis1,  &
      & coefOnAxis2=coefOnAxis2)
  END IF

  IF (ALLOCATED(p1)) DEALLOCATE (p1)
  IF (ALLOCATED(p2)) DEALLOCATE (p2)
  IF (ALLOCATED(p3)) DEALLOCATE (p3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE SetGmshStructuredMeshParam2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Check essential parameters

SUBROUTINE mesh_CheckEssentialParam(obj, param)
  CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "mesh_CheckEssentialParam()"
  TYPE(String) :: astr
  TYPE(String), ALLOCATABLE :: essentialParam(:)
  INTEGER(I4B) :: ii

  astr = "/filename/tVolumes/tPoints/pointsOnAxis1/pointsOnAxis2/"// &
  & "pointsOnAxis3/transfinitePointsOnAxis1/transfinitePointsOnAxis2/"// &
  & "transfinitePointsOnAxis3/recombineAll/meshTypeOnAxis1/"// &
  & "meshTypeOnAxis2/meshTypeOnAxis3/coefOnAxis1/coefOnAxis2/"// &
  & "coefOnAxis3/nsd"

  CALL astr%Split(essentialParam, sep="/")

  CALL CheckEssentialParam( &
    & obj=param,  &
    & keys=essentialParam,  &
    & prefix=myprefix,  &
    & myName=myName,  &
    & modName=modName)

  IF (ALLOCATED(essentialParam)) THEN
    DO ii = 1, SIZE(essentialParam)
      essentialParam(ii) = ""
    END DO
    DEALLOCATE (essentialParam)
  END IF

  astr = ""

END SUBROUTINE mesh_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Initiate the object

SUBROUTINE mesh_Initiate(obj, param)
  CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param

  ! internal variables
  INTEGER(I4B) :: ii, jj, kk, ipoint, iedge, lineLoop(4), tedges(3),  &
    & isurface, te(3), p(2), tsurfaces(3), iVolume, surfaceLoops(6)
  REAL(DFP), ALLOCATABLE :: pointsOnAxis1(:, :), pointsOnAxis2(:, :),  &
    & pointsOnAxis3(:, :), coefOnAxis1(:), coefOnAxis2(:), coefOnAxis3(:),  &
    & dummy_real_r2(:, :)
  INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis1(:), meshTypeOnAxis2(:),  &
    & meshTypeOnAxis3(:)
  CHARACTER(*), PARAMETER :: myName = "mesh_Initiate()"

  CALL obj%DEALLOCATE()

  CALL obj%CheckEssentialParam(param)

  CALL GetValue(obj=param, prefix=myprefix, key="recombineAll",  &
    & VALUE=obj%recombineAll)

  CALL GetValue(obj=param, prefix=myprefix, key="filename",  &
    & VALUE=obj%filename)

  CALL GetValue(obj=param, prefix=myprefix, key="tVolumes",  &
    & VALUE=obj%tVolumes)

  CALL GetValue(obj=param, prefix=myprefix, key="tPoints",  &
    & VALUE=obj%tPoints)

  CALL GetValue(obj=param, prefix=myprefix, key="nsd",  &
    & VALUE=obj%nsd)

  te = obj%tPoints - 1

  DO ii = 1, 3
    CALL Initiate(obj%points(ii), 3_I4B, obj%tPoints(ii))
    CALL GetValue(obj=param, prefix=myprefix,  &
      & key="pointsOnAxis"//tostring(ii), VALUE=obj%points(ii))
  END DO

  ALLOCATE (pointsOnAxis1(3, 0:obj%tPoints(1)))
  ALLOCATE (pointsOnAxis2(3, 0:obj%tPoints(2)))
  ALLOCATE (pointsOnAxis3(3, 0:obj%tPoints(3)))
  pointsOnAxis1 = 0.0_DFP
  pointsOnAxis2 = 0.0_DFP
  pointsOnAxis3 = 0.0_DFP

  pointsOnAxis1(1:3, 1:) = Get(obj%points(1))
  pointsOnAxis2(1:3, 1:) = Get(obj%points(2))
  pointsOnAxis3(1:3, 1:) = Get(obj%points(3))

  ipoint = obj%tPoints(1) * obj%tPoints(2) * obj%tPoints(3)
  CALL Reallocate(obj%allPoints, 3_I4B, ipoint)

  ipoint = 0
  DO kk = 1, obj%tpoints(3)
    DO jj = 1, obj%tPoints(2)
      DO ii = 1, obj%tPoints(1)
        ipoint = ipoint + 1
        ! obj%allPoints(:, ipoint) = [pointsOnAxis1(1, ii),  &
        !   & pointsOnAxis2(2, jj),  &
        !   & pointsOnAxis3(3, kk)]
        obj%allPoints(:, ipoint) = pointsOnAxis1(:, ii - 1) &
          & + pointsOnAxis1(:, ii) - pointsOnAxis1(:, ii - 1) &
          & + pointsOnAxis2(:, jj) - pointsOnAxis2(:, jj - 1) &
          & + pointsOnAxis3(:, kk) - pointsOnAxis3(:, kk - 1)
      END DO
    END DO
  END DO

  CALL Reallocate(meshTypeOnAxis1, MAX(te(1), 1_I4B))
  CALL Reallocate(meshTypeOnAxis2, MAX(te(2), 1_I4B))
  CALL Reallocate(meshTypeOnAxis3, MAX(te(3), 1_I4B))

  CALL Reallocate(coefOnAxis1, MAX(te(1), 1_I4B))
  CALL Reallocate(coefOnAxis2, MAX(te(2), 1_I4B))
  CALL Reallocate(coefOnAxis3, MAX(te(3), 1_I4B))

  CALL GetValue(obj=param, prefix=myprefix,  &
    & key="meshTypeOnAxis1", VALUE=meshTypeOnAxis1)

  CALL GetValue(obj=param, prefix=myprefix,  &
    & key="meshTypeOnAxis2", VALUE=meshTypeOnAxis2)

  CALL GetValue(obj=param, prefix=myprefix,  &
    & key="meshTypeOnAxis3", VALUE=meshTypeOnAxis3)

  CALL GetValue(obj=param, prefix=myprefix,  &
    & key="coefOnAxis1", VALUE=coefOnAxis1)

  CALL GetValue(obj=param, prefix=myprefix,  &
    & key="coefOnAxis2", VALUE=coefOnAxis2)

  CALL GetValue(obj=param, prefix=myprefix,  &
    & key="coefOnAxis3", VALUE=coefOnAxis3)

  DO ii = 1, 3
    CALL Initiate(obj%transfinitePoints(ii),  &
      & MAX(te(ii), 1_I4B))
    CALL GetValue(obj=param, prefix=myprefix,  &
      & key="transfinitePointsOnAxis"//tostring(ii),  &
      & VALUE=obj%transfinitePoints(ii))
  END DO

  tedges(1) = te(1) * obj%tPoints(2) * obj%tPoints(3)
  tedges(2) = te(2) * obj%tPoints(3) * obj%tPoints(1)
  tedges(3) = te(3) * obj%tPoints(2) * obj%tPoints(1)

  obj%tEdges1 = tedges(1)
  obj%tEdges2 = tedges(2)
  obj%tEdges3 = tedges(3)

  obj%tEdges = tedges(1) + tedges(2) + tedges(3)

  CALL Reallocate(obj%edges, 2, obj%tEdges)
  CALL Reallocate(obj%edge_tfp, obj%tEdges)
  CALL Reallocate(obj%edge_meshType, obj%tEdges)
  CALL Reallocate(obj%edge_coef, obj%tEdges)

  ! edges parallel to axis 1
  iedge = 0
  DO kk = 1, obj%tPoints(3)
    DO jj = 1, obj%tPoints(2)
      DO ii = 1, obj%tPoints(1) - 1
        p(1) = obj%GetNodeNumber(ii, jj, kk)
        p(2) = obj%GetNodeNumber(ii + 1, jj, kk)
        iedge = iedge + 1
        obj%edges(:, iedge) = p
        obj%edge_tfp(iedge) = Get(obj%transfinitePoints(1), ii, 1_I4B)
        obj%edge_coef(iedge) = coefOnAxis1(ii)
        obj%edge_meshType(iedge) = MeshTypeName(meshTypeOnAxis1(ii))
      END DO
    END DO
  END DO

  ! edges parallel to axis 2
  DO kk = 1, obj%tPoints(3)
    DO jj = 1, obj%tPoints(2) - 1
      DO ii = 1, obj%tPoints(1)
        p(1) = obj%GetNodeNumber(ii, jj, kk)
        p(2) = obj%GetNodeNumber(ii, jj + 1, kk)
        iedge = iedge + 1
        obj%edges(:, iedge) = p
        obj%edge_tfp(iedge) = Get(obj%transfinitePoints(2), jj, 1_I4B)
        obj%edge_coef(iedge) = coefOnAxis2(jj)
        obj%edge_meshType(iedge) = MeshTypeName(meshTypeOnAxis2(jj))
      END DO
    END DO
  END DO

  ! edges parallel to axis 3
  DO kk = 1, obj%tPoints(3) - 1
    DO jj = 1, obj%tPoints(2)
      DO ii = 1, obj%tPoints(1)
        p(1) = obj%GetNodeNumber(ii, jj, kk)
        p(2) = obj%GetNodeNumber(ii, jj, kk + 1)
        iedge = iedge + 1
        obj%edges(:, iedge) = p
        obj%edge_tfp(iedge) = Get(obj%transfinitePoints(3), kk, 1_I4B)
        obj%edge_coef(iedge) = coefOnAxis3(kk)
        obj%edge_meshType(iedge) = MeshTypeName(meshTypeOnAxis3(kk))
      END DO
    END DO
  END DO

  ! xy
  tsurfaces(1) = te(1) * te(2) * obj%tPoints(3)
  ! yz
  tsurfaces(2) = te(2) * te(3) * obj%tPoints(1)
  ! xz
  tsurfaces(3) = te(1) * te(3) * obj%tPoints(2)

  obj%tSurfacesXY = tsurfaces(1)
  obj%tSurfacesYZ = tsurfaces(2)
  obj%tSurfacesXZ = tsurfaces(3)

  obj%tSurfaces = tsurfaces(1) + tsurfaces(2) + tsurfaces(3)

  CALL Reallocate(obj%curveLoops, 4, obj%tSurfaces)

  isurface = 0
  ! xy (1-2)
  DO kk = 1, obj%tPoints(3)
    DO jj = 1, obj%tPoints(2) - 1
      DO ii = 1, obj%tPoints(1) - 1
        isurface = isurface + 1
        lineLoop(1) = obj%GetEdgeNumberOnAxis1(ii, jj, kk)
        lineLoop(3) = obj%GetEdgeNumberOnAxis1(ii, jj + 1, kk)
        lineLoop(2) = obj%GetEdgeNumberOnAxis2(ii + 1, jj, kk)
        lineLoop(4) = obj%GetEdgeNumberOnAxis2(ii, jj, kk)
        obj%curveLoops(:, isurface) = [lineLoop(1), lineLoop(2), &
          & -lineLoop(3), -lineLoop(4)]
      END DO
    END DO
  END DO

  ! yz (2-3)
  DO ii = 1, obj%tPoints(1)
    DO kk = 1, obj%tPoints(3) - 1
      DO jj = 1, obj%tPoints(2) - 1
        isurface = isurface + 1
        lineLoop(1) = obj%GetEdgeNumberOnAxis2(ii, jj, kk)
        lineLoop(3) = obj%GetEdgeNumberOnAxis2(ii, jj, kk + 1)
        lineLoop(2) = obj%GetEdgeNumberOnAxis3(ii, jj + 1, kk)
        lineLoop(4) = obj%GetEdgeNumberOnAxis3(ii, jj, kk)
        obj%curveLoops(:, isurface) = [lineLoop(1), lineLoop(2), &
          & -lineLoop(3), -lineLoop(4)]
      END DO
    END DO
  END DO

  ! xz (1-3)
  DO jj = 1, obj%tPoints(2)
    DO kk = 1, obj%tPoints(3) - 1
      DO ii = 1, obj%tPoints(1) - 1
        isurface = isurface + 1

        lineLoop(1) = obj%GetEdgeNumberOnAxis1(ii, jj, kk)
        lineLoop(3) = obj%GetEdgeNumberOnAxis1(ii, jj, kk + 1)
        lineLoop(2) = obj%GetEdgeNumberOnAxis3(ii + 1, jj, kk)
        lineLoop(4) = obj%GetEdgeNumberOnAxis3(ii, jj, kk)

        obj%curveLoops(:, isurface) = [lineLoop(1), lineLoop(2), &
          & -lineLoop(3), -lineLoop(4)]
      END DO
    END DO
  END DO

  ! making volumes
  IF (obj%tVolumes .GT. 0_I4B) THEN
    CALL Reallocate(obj%surfaceLoops, 6_I4B, obj%tVolumes)
  ELSE
    CALL Reallocate(obj%surfaceLoops, 0_I4B, 0_I4B)
  END IF

  iVolume = 0_I4B
  DO kk = 1, obj%tPoints(3) - 1
    DO jj = 1, obj%tPoints(2) - 1
      DO ii = 1, obj%tPoints(1) - 1
        iVolume = iVolume + 1
        surfaceLoops(1) = obj%GetSurfaceNumberXY(ii, jj, kk)
        surfaceLoops(2) = obj%GetSurfaceNumberXY(ii, jj, kk + 1)

        surfaceLoops(3) = obj%GetSurfaceNumberYZ(ii, jj, kk)
        surfaceLoops(4) = obj%GetSurfaceNumberYZ(ii + 1, jj, kk)

        surfaceLoops(5) = obj%GetSurfaceNumberXZ(ii, jj, kk)
        surfaceLoops(6) = obj%GetSurfaceNumberXZ(ii, jj + 1, kk)
        obj%surfaceLoops(:, iVolume) = surfaceLoops
      END DO
    END DO
  END DO

  IF (ALLOCATED(pointsOnAxis1)) DEALLOCATE (pointsOnAxis1)
  IF (ALLOCATED(pointsOnAxis2)) DEALLOCATE (pointsOnAxis2)
  IF (ALLOCATED(pointsOnAxis3)) DEALLOCATE (pointsOnAxis3)

  IF (ALLOCATED(coefOnAxis1)) DEALLOCATE (coefOnAxis1)
  IF (ALLOCATED(coefOnAxis2)) DEALLOCATE (coefOnAxis2)
  IF (ALLOCATED(coefOnAxis3)) DEALLOCATE (coefOnAxis3)

  IF (ALLOCATED(meshTypeOnAxis1)) DEALLOCATE (meshTypeOnAxis1)
  IF (ALLOCATED(meshTypeOnAxis2)) DEALLOCATE (meshTypeOnAxis2)
  IF (ALLOCATED(meshTypeOnAxis3)) DEALLOCATE (meshTypeOnAxis3)

END SUBROUTINE mesh_Initiate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Deallocate data

SUBROUTINE mesh_Deallocate(obj)
  CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: ii

  DO ii = 1, SIZE(obj%points)
    CALL DEALLOCATE (obj%points(ii))
  END DO

  DO ii = 1, 3
    CALL DEALLOCATE (obj%transfinitePoints(ii))
  END DO
  obj%tPoints = 0

  IF (ALLOCATED(obj%allPoints)) DEALLOCATE (obj%allPoints)

  IF (ALLOCATED(obj%edges)) DEALLOCATE (obj%edges)
  IF (ALLOCATED(obj%edge_tfp)) DEALLOCATE (obj%edge_tfp)
  IF (ALLOCATED(obj%edge_coef)) DEALLOCATE (obj%edge_coef)
  IF (ALLOCATED(obj%edge_meshType)) DEALLOCATE (obj%edge_meshType)
  obj%tEdges = 0

  IF (ALLOCATED(obj%curveLoops)) DEALLOCATE (obj%curveLoops)
  obj%tSurfaces = 0

END SUBROUTINE mesh_Deallocate

!----------------------------------------------------------------------------
!                                                                 Generate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  Generate mesh

SUBROUTINE mesh_Generate(obj, gmsh)
  CLASS(GmshStructuredMesh_), INTENT(inout) :: obj
  CLASS(Gmsh_), INTENT(INOUT) :: gmsh

  ! internal variables
  INTEGER(I4B) :: ierr

  CALL obj%GeneratePoints(gmsh)
  CALL obj%GenerateCurves(gmsh)
  CALL obj%GenerateSurfaces(gmsh)
  CALL obj%GenerateVolumes(gmsh)
  ierr = gmsh%model%geo%Synchronize()
  ierr = gmsh%option%SetNumber(name="Mesh.SaveAll", VALUE=1_I4B)
  ierr = gmsh%model%mesh%Generate(obj%nsd)
  ierr = gmsh%WRITE(obj%filename%chars())
END SUBROUTINE mesh_Generate

!----------------------------------------------------------------------------
!                                                             GeneratePoints
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate points

SUBROUTINE mesh_GeneratePoints(obj, gmsh)
  CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
  CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  ! internal variables
  INTEGER(I4B) :: ipoint, ierr

  DO ipoint = 1, SIZE(obj%allPoints, 2)
    ierr = gmsh%model%geo%addPoint( &
      & x=obj%allPoints(1, ipoint),  &
      & y=obj%allPoints(2, ipoint),  &
      & z=obj%allPoints(3, ipoint),  &
      & meshSize=1.0_DFP)
  END DO
END SUBROUTINE mesh_GeneratePoints

!----------------------------------------------------------------------------
!                                                             GenerateCurves
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate curves

SUBROUTINE mesh_GenerateCurves(obj, gmsh)
  CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
  CLASS(Gmsh_), INTENT(INOUT) :: gmsh

  ! internal variables
  INTEGER(I4B) :: iedge, ierr

  DO iedge = 1, obj%tEdges
    ierr = gmsh%model%geo%AddLine(obj%edges(1, iedge), obj%edges(2, iedge))
    ierr = gmsh%model%geo%mesh%SetTransfiniteCurve(tag=iedge,  &
      & nPoints=obj%edge_tfp(iedge),  &
      & meshType=obj%edge_meshType(iedge)%chars(),  &
      & coef=obj%edge_coef(iedge))
  END DO

END SUBROUTINE mesh_GenerateCurves

!----------------------------------------------------------------------------
!                                                           GenerateSurfaces
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate Surfaces

SUBROUTINE mesh_GenerateSurfaces(obj, gmsh)
  CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
  CLASS(Gmsh_), INTENT(INOUT) :: gmsh

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "mesh_GenerateSurfaces()"
  INTEGER(I4B) :: isurf, ierr

  DO isurf = 1, obj%tSurfaces
    ierr = gmsh%model%geo%AddCurveLoop( &
      & curveTags=obj%curveLoops(:, isurf),  &
      & reorient=.TRUE.)
  END DO

  DO isurf = 1, obj%tSurfaces
    ierr = gmsh%model%geo%AddPlaneSurface(wireTags=[isurf])
    ierr = gmsh%model%geo%mesh%SetTransfiniteSurface(tag=isurf)
  END DO

  IF (obj%recombineAll) THEN
    DO isurf = 1, obj%tSurfaces
      ierr = gmsh%model%geo%mesh%SetRecombine(dim=2_I4B, tag=isurf)
    END DO
  END IF
END SUBROUTINE mesh_GenerateSurfaces

!----------------------------------------------------------------------------
!                                                           GenerateVolumes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate Volumes

SUBROUTINE mesh_GenerateVolumes(obj, gmsh)
  CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
  CLASS(Gmsh_), INTENT(INOUT) :: gmsh

  CHARACTER(*), PARAMETER :: myName = "mesh_GenerateVolumes"

  ! Internal variables
  INTEGER(I4B) :: iVolume, ierr

  DO iVolume = 1, obj%tVolumes
    ierr = gmsh%model%geo%AddSurfaceLoop( &
      & surfaceTags=obj%surfaceLoops(:, iVolume))
  END DO

  DO iVolume = 1, obj%tVolumes
    ierr = gmsh%model%geo%AddVolume(shellTags=[iVolume])
    ierr = gmsh%model%geo%mesh%SetTransfiniteVolume(tag=iVolume)
  END DO

  IF (obj%recombineAll) THEN
    DO iVolume = 1, obj%tVolumes
      ierr = gmsh%model%geo%mesh%SetRecombine(dim=3_I4B, tag=iVolume)
    END DO
  END IF
END SUBROUTINE mesh_GenerateVolumes

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Display the content

SUBROUTINE mesh_Display(obj, msg, unitNo)
  CLASS(GmshStructuredMesh_), INTENT(inout) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo

  ! internal variables
  IF (obj%recombineAll) THEN
    CALL Display("recombineAll : TRUE", unitNo=unitNo)
  ELSE
    CALL Display("recombineAll : FALSE", unitNo=unitNo)
  END IF

  CALL Display(obj%nsd, "nsd : ", unitNo=unitNo)
  CALL Display(TRIM(obj%filename), "filename : ", unitNo=unitNo)
  CALL Display(obj%tVolumes, "tVolumes : ", unitNo=unitNo)
  CALL Display(obj%tPoints, "tPoints : ", unitNo=unitNo)
  CALL Display(obj%points(1), "pointsOnAxis1 : ", unitNo=unitNo)
  CALL Display(obj%points(2), "pointsOnAxis2 : ", unitNo=unitNo)
  CALL Display(obj%points(3), "pointsOnAxis3 : ", unitNo=unitNo)
  CALL Display(obj%allPoints, "All Points : ", unitNo=unitNo)

  CALL Display(obj%transfinitePoints(1), "transfinitePointsOnAxis1: ", &
    & unitNo=unitNo)
  CALL Display(obj%transfinitePoints(2), "transfinitePointsOnAxis2: ", &
    & unitNo=unitNo)
  CALL Display(obj%transfinitePoints(3), "transfinitePointsOnAxis3: ", &
    & unitNo=unitNo)
  CALL Display(obj%edge_tfp, "Transfinite Points : ", &
    & unitNo=unitNo)
  CALL Display(obj%edge_coef, "Transfinite coef : ", &
    & unitNo=unitNo)
  ! CALL Display(obj%edge_meshType, "Transfinite meshType : ", &
  !   & unitNo=unitNo)
  CALL Display(obj%edges, "Edges : ", &
    & unitNo=unitNo)
  CALL Display(obj%tEdges, "Total edges : ", &
    & unitNo=unitNo)
  CALL Display(obj%curveLoops, "SurfaceLoops : ", &
    & unitNo=unitNo)
  CALL Display(obj%tSurfaces, "Total surfaces : ", &
    & unitNo=unitNo)
  CALL Display(obj%edges, "edges : ", unitNo=unitNo)
END SUBROUTINE mesh_Display

!----------------------------------------------------------------------------
!                                                             GetNodeNumber
!----------------------------------------------------------------------------

FUNCTION mesh_GetNodeNumber(obj, i, j, k) RESULT(ans)
  CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: i, j, k
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: ni, nj, nk, ii, jj, kk

  ii = i; jj = j; kk = k; ni = obj%tPoints(1); nj = obj%tPoints(2)
  nk = obj%tPoints(3)
  ans = (j - 1) * ni + i + (k - 1) * ni * nj
END FUNCTION mesh_GetNodeNumber

!----------------------------------------------------------------------------
!                                                      GetEdgeNumberOnAxis1
!----------------------------------------------------------------------------

FUNCTION mesh_GetEdgeNumberOnAxis1(obj, i, j, k) RESULT(ans)
  CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: i, j, k
  !! point number
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: ii, jj, kk, ni, nj, nk

  ii = i; jj = j; kk = k
  ni = obj%tPoints(1) - 1; nj = obj%tPoints(2) - 1; nk = obj%tPoints(3) - 1

  ans = (jj - 1) * ni + ii + ni * (nj + 1) * (kk - 1)

END FUNCTION mesh_GetEdgeNumberOnAxis1

!----------------------------------------------------------------------------
!                                                      GetEdgeNumberOnAxis2
!----------------------------------------------------------------------------

FUNCTION mesh_GetEdgeNumberOnAxis2(obj, i, j, k) RESULT(ans)
  CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: i, j, k
  !! point number
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: ii, jj, kk, ni, nj, nk

  ii = i; jj = j; kk = k
  ni = obj%tPoints(1) - 1; nj = obj%tPoints(2) - 1; nk = obj%tPoints(3) - 1

  ans = obj%tEdges1 + (jj - 1) * (ni + 1) + ii + (ni + 1) * nj * (kk - 1)
END FUNCTION mesh_GetEdgeNumberOnAxis2

!----------------------------------------------------------------------------
!                                                      GetEdgeNumberOnAxis3
!----------------------------------------------------------------------------

FUNCTION mesh_GetEdgeNumberOnAxis3(obj, i, j, k) RESULT(ans)
  CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: i, j, k
  !! cell number
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: ii, jj, kk, ni, nj, nk

  ii = i; jj = j; kk = k
  ni = obj%tPoints(1) - 1; nj = obj%tPoints(2) - 1; nk = obj%tPoints(3) - 1

  ans = obj%tEdges1 + obj%tEdges2   &
    & + (jj - 1) * (ni + 1) + ii + (ni + 1) * (nj + 1) * (kk - 1)
END FUNCTION mesh_GetEdgeNumberOnAxis3

!----------------------------------------------------------------------------
!                                                      GetSurfaceNumberXY
!----------------------------------------------------------------------------

FUNCTION mesh_GetSurfaceNumberXY(obj, i, j, k) RESULT(ans)
  CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: i, j, k
  !! point number
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: ii, jj, kk, ni, nj, nk

  ii = i; jj = j; kk = k
  ni = obj%tPoints(1) - 1; nj = obj%tPoints(2) - 1; nk = obj%tPoints(3) - 1

  ans = (jj - 1) * ni + ii + ni * nj * (kk - 1)

END FUNCTION mesh_GetSurfaceNumberXY

!----------------------------------------------------------------------------
!                                                      GetSurfaceNumberYZ
!----------------------------------------------------------------------------

FUNCTION mesh_GetSurfaceNumberYZ(obj, i, j, k) RESULT(ans)
  CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: i, j, k
  !! point number
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: ii, jj, kk, ni, nj, nk

  ii = i; jj = j; kk = k
  ni = obj%tPoints(1) - 1; nj = obj%tPoints(2) - 1; nk = obj%tPoints(3) - 1

  ans = obj%tSurfacesXY + (kk - 1) * nj + jj + nj * nk * (ii - 1)

END FUNCTION mesh_GetSurfaceNumberYZ

!----------------------------------------------------------------------------
!                                                      GetSurfaceNumberXZ
!----------------------------------------------------------------------------

FUNCTION mesh_GetSurfaceNumberXZ(obj, i, j, k) RESULT(ans)
  CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: i, j, k
  !! point number
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: ii, jj, kk, ni, nj, nk

  ii = i; jj = j; kk = k
  ni = obj%tPoints(1) - 1; nj = obj%tPoints(2) - 1; nk = obj%tPoints(3) - 1

  ans = obj%tSurfacesXY + obj%tSurfacesYZ +  &
    & (kk - 1) * ni + ii + ni * nk * (jj - 1)

END FUNCTION mesh_GetSurfaceNumberXZ

END MODULE GmshStructuredMesh_Class

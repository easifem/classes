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

MODULE GmshStructuredMesh3_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: math => TypeMathOpt
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE Gmsh_Class, ONLY: Gmsh_
USE BaseType, ONLY: RealMatrix_
USE BaseType, ONLY: IntVector_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshStructuredMesh_
PUBLIC :: GmshStructuredMeshPointer_

INTEGER(I4B), PARAMETER :: Progression = 1
INTEGER(I4B), PARAMETER :: Bump = 2
INTEGER(I4B), PUBLIC, PARAMETER :: GMSH_API_PROGRESSION = 1
INTEGER(I4B), PUBLIC, PARAMETER :: GMSH_API_BUMP = 2
CHARACTER(*), PARAMETER :: myprefix = "StructuredMesh"
CHARACTER(*), PARAMETER :: MeshTypeName(2) = ["Progression", "Bump       "]

!----------------------------------------------------------------------------
!                                                      GmshStructuredMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  The data type to create structured mesh by using Gmsh

TYPE :: GmshStructuredMesh_
  ! PRIVATE
  LOGICAL(LGT) :: recombineAll = math%yes
  !! All surfaces will be recombine into quad or hexahedron
  INTEGER(I4B) :: nsd = math%two_i
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
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => mesh_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => mesh_Initiate2
  GENERIC, PUBLIC :: Initate => Initiate1, Initiate2
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => mesh_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Generate => mesh_Generate
  PROCEDURE, PUBLIC, PASS(obj) :: GeneratePoints => mesh_GeneratePoints
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateCurves => mesh_GenerateCurves
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateSurfaces => mesh_GenerateSurfaces
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateVolumes => mesh_GenerateVolumes
  PROCEDURE, PUBLIC, PASS(obj) :: Display => mesh_Display
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeNumber => mesh_GetNodeNumber
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeNumberOnAxis1 => &
    mesh_GetEdgeNumberOnAxis1
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeNumberOnAxis2 => &
    mesh_GetEdgeNumberOnAxis2
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeNumberOnAxis3 => &
    mesh_GetEdgeNumberOnAxis3
  PROCEDURE, PUBLIC, PASS(obj) :: GetSurfaceNumberXY => &
    mesh_GetSurfaceNumberXY
  PROCEDURE, PUBLIC, PASS(obj) :: GetSurfaceNumberYZ => &
    mesh_GetSurfaceNumberYZ
  PROCEDURE, PUBLIC, PASS(obj) :: GetSurfaceNumberXZ => &
    mesh_GetSurfaceNumberXZ
END TYPE GmshStructuredMesh_

!----------------------------------------------------------------------------
!                                                 GmshStructuredMeshPointer_
!----------------------------------------------------------------------------

TYPE :: GmshStructuredMeshPointer_
  CLASS(GmshStructuredMesh_), POINTER :: ptr => NULL()
END TYPE GmshStructuredMeshPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Initiate the object

INTERFACE
  MODULE SUBROUTINE mesh_Initiate1( &
    obj, filename, pointsOnAxis1, transfinitePointsOnAxis1, &
    pointsOnAxis2, transfinitePointsOnAxis2, pointsOnAxis3, &
    transfinitePointsOnAxis3, recombineAll, meshTypeOnAxis1, &
    meshTypeOnAxis2, meshTypeOnAxis3, coefOnAxis1, &
    coefOnAxis2, coefOnAxis3)
    CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
    !! Gmsh structured mesh
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
  END SUBROUTINE mesh_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Initiate the object

INTERFACE
  MODULE SUBROUTINE mesh_Initiate2( &
    obj, filename, pointsOnAxis1, transfinitePointsOnAxis1, &
    pointsOnAxis2, transfinitePointsOnAxis2, pointsOnAxis3, &
    transfinitePointsOnAxis3, recombineAll, meshTypeOnAxis1, &
    meshTypeOnAxis2, meshTypeOnAxis3, coefOnAxis1, coefOnAxis2, &
    coefOnAxis3)
    CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
    !! structured mesh
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
  END SUBROUTINE mesh_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Deallocate data

INTERFACE
  MODULE SUBROUTINE mesh_Deallocate(obj)
    CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ii
  END SUBROUTINE mesh_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Generate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  Generate mesh

INTERFACE
  MODULE SUBROUTINE mesh_Generate(obj, gmsh)
    CLASS(GmshStructuredMesh_), INTENT(inout) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE mesh_Generate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GeneratePoints
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate points

INTERFACE
  MODULE SUBROUTINE mesh_GeneratePoints(obj, gmsh)
    CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE mesh_GeneratePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GenerateCurves
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate curves

INTERFACE
  MODULE SUBROUTINE mesh_GenerateCurves(obj, gmsh)
    CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE mesh_GenerateCurves
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GenerateSurfaces
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate Surfaces

INTERFACE
  MODULE SUBROUTINE mesh_GenerateSurfaces(obj, gmsh)
    CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE mesh_GenerateSurfaces
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GenerateVolumes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate Volumes

INTERFACE
  MODULE SUBROUTINE mesh_GenerateVolumes(obj, gmsh)
    CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE mesh_GenerateVolumes
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Display the content

INTERFACE
  MODULE SUBROUTINE mesh_Display(obj, msg, unitNo)
    CLASS(GmshStructuredMesh_), INTENT(inout) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE mesh_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetNodeNumber
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION mesh_GetNodeNumber(obj, i, j, k) RESULT(ans)
    CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j, k
    INTEGER(I4B) :: ans
  END FUNCTION mesh_GetNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetEdgeNumberOnAxis1
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION mesh_GetEdgeNumberOnAxis1(obj, i, j, k) RESULT(ans)
    CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j, k
    !! point number
    INTEGER(I4B) :: ans
  END FUNCTION mesh_GetEdgeNumberOnAxis1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetEdgeNumberOnAxis2
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION mesh_GetEdgeNumberOnAxis2(obj, i, j, k) RESULT(ans)
    CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j, k
  !! point number
    INTEGER(I4B) :: ans
  END FUNCTION mesh_GetEdgeNumberOnAxis2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetEdgeNumberOnAxis3
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION mesh_GetEdgeNumberOnAxis3(obj, i, j, k) RESULT(ans)
    CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j, k
    !! cell number
    INTEGER(I4B) :: ans
  END FUNCTION mesh_GetEdgeNumberOnAxis3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetSurfaceNumberXY
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION mesh_GetSurfaceNumberXY(obj, i, j, k) RESULT(ans)
    CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j, k
    !! point number
    INTEGER(I4B) :: ans
  END FUNCTION mesh_GetSurfaceNumberXY
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetSurfaceNumberYZ
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION mesh_GetSurfaceNumberYZ(obj, i, j, k) RESULT(ans)
    CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j, k
  !! point number
    INTEGER(I4B) :: ans
  END FUNCTION mesh_GetSurfaceNumberYZ
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetSurfaceNumberXZ
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION mesh_GetSurfaceNumberXZ(obj, i, j, k) RESULT(ans)
    CLASS(GmshStructuredMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j, k
    !! point number
    INTEGER(I4B) :: ans
  END FUNCTION mesh_GetSurfaceNumberXZ
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshStructuredMesh_Class

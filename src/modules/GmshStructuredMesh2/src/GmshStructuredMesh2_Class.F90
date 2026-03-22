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

MODULE GmshStructuredMesh2_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: math => TypeMathOpt
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE Gmsh_Class, ONLY: Gmsh_
USE BaseType, ONLY: RealMatrix_
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: RealVector_
USE Tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshStructuredMesh2_
PUBLIC :: GmshStructuredMesh2Pointer_

INTEGER(I4B), PARAMETER :: Progression = 1
INTEGER(I4B), PARAMETER :: Bump = 2

!----------------------------------------------------------------------------
!                                                      GmshStructuredMesh2_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  The data type to create structured mesh by using Gmsh

TYPE :: GmshStructuredMesh2_
  ! PRIVATE
  LOGICAL(LGT) :: recombineAll = math%yes
  !! All surfaces will be recombine into quad or hexahedron
  TYPE(String) :: filename
  !! Name of the mesh file to be generated
  TYPE(RealMatrix_) :: points(2)
  !! points on axis 1, axis 2, axis 3
  !! points(1) are points on axis 1
  !! points(2) are points on axis 2
  INTEGER(I4B) :: tPoints(2) = 0
  !! Total points on axis 1 to define the region
  !! Total points on axis 2 to define the region
  REAL(DFP), ALLOCATABLE :: allPoints(:, :)
  !! All the points
  TYPE(IntVector_) :: transfinitePoints(3)
  !! transfinitePoints on lines
  !! transfinitePoints(1) is transfinitePoints on axis 1
  !! transfinitePoints(2) is transfinitePoints on axis 2
  TYPE(RealVector_) :: transfiniteCoeff(3)
  !! coefficient for transfinite segments on axis
  TYPE(IntVector_) :: transfiniteMeshType(3)
  !! meshtype for transfinite segments on axis
  INTEGER(I4B), ALLOCATABLE :: edge_tfp(:)
  !! transfinitePoints for edges
  REAL(DFP), ALLOCATABLE :: edge_coeff(:)
  !! coeffiecient for transfinite points on edges
  TYPE(String), ALLOCATABLE :: edge_meshType(:)
  !! mesh type for transfinite curves
  INTEGER(I4B), ALLOCATABLE :: edges(:, :)
  !! The edges
  INTEGER(I4B) :: tEdges1 = 0
  !! Total edges parallel to axis 1
  INTEGER(I4B) :: tEdges2 = 0
  !! Total edges parallel to axis 2
  INTEGER(I4B) :: tEdges = 0
  !! Total number of edges = tEdges1+tEdges2
  INTEGER(I4B), ALLOCATABLE :: curveLoops(:, :)
  !! Surface loops for boxes, it means boxes in terms of
  !! edges
  INTEGER(I4B) :: tSurfaces = 0
  !! Total number of surfaces
  !! If there are no holes then tSurfaces equal to tVolumes

CONTAINS
  PRIVATE

  !@ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate

  !@GenerateMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Generate => obj_Generate
  PROCEDURE, PUBLIC, PASS(obj) :: GeneratePoints => obj_GeneratePoints
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateCurves => obj_GenerateCurves
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateSurfaces => obj_GenerateSurfaces

  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  !@GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeNumber => obj_GetNodeNumber
  !! Get the node number
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeNumberOnAxis1 => &
    obj_GetEdgeNumberOnAxis1
  !! Get the edge number on axis1
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeNumberOnAxis2 => &
    obj_GetEdgeNumberOnAxis2
  !! Get the edge number on axis2
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshTypeName => &
    obj_GetMeshTypeName
  !! get the meshtype name from integer

  !@TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import obj from toml table
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import obj from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Generic method for importing obj from toml
END TYPE GmshStructuredMesh2_

!----------------------------------------------------------------------------
!                                                 GmshStructuredMesh2Pointer_
!----------------------------------------------------------------------------

TYPE :: GmshStructuredMesh2Pointer_
  CLASS(GmshStructuredMesh2_), POINTER :: ptr => NULL()
END TYPE GmshStructuredMesh2Pointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Initiate the object

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, filename, pointsOnAxis1, transfinitePointsOnAxis1, &
    pointsOnAxis2, transfinitePointsOnAxis2, recombineAll, &
    meshTypeOnAxis1, meshTypeOnAxis2, coeffOnAxis1, coeffOnAxis2)
    CLASS(GmshStructuredMesh2_), INTENT(INOUT) :: obj
    !! Gmsh structured mesh
    CHARACTER(*), INTENT(IN) :: filename
    !! name of the mesh file to be generated
    REAL(DFP), INTENT(IN) :: pointsOnAxis1(:, :)
    !! points on axis 1
    INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis1(:)
    !! transfinitePoints on axis 1, transfinitePointsOnAxis1(ii) denotes
    !! the transfinite points on segament ii on axis1, the size of
    !! this vector should be 1 less than the number of points on axis1
    REAL(DFP), INTENT(IN) :: pointsOnAxis2(:, :)
    !! points on axis 2
    INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis2(:)
    !! transfinitePoints on axis 2, transfinitePointsOnAxis2(ii) denotes
    !! the transfinite points on segament ii on axis2, the size of
    !! this vector should be 1 less than the number of points on axis2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombineAll
    !! If true we combine triangle and tetrahedron into quad and hexahedron
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis1(:)
    !! mesh type on each segment on axis 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis2(:)
    !! mesh type on each segment on axis 2
    REAL(DFP), OPTIONAL, INTENT(IN) :: coeffOnAxis1(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: coeffOnAxis2(:)
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Deallocate data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(GmshStructuredMesh2_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ii
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Generate@GenerateMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  Generate mesh

INTERFACE
  MODULE SUBROUTINE obj_Generate(obj, gmsh)
    CLASS(GmshStructuredMesh2_), INTENT(inout) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_Generate
END INTERFACE

!----------------------------------------------------------------------------
!                                             GeneratePoints@GenerateMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate points

INTERFACE
  MODULE SUBROUTINE obj_GeneratePoints(obj, gmsh)
    CLASS(GmshStructuredMesh2_), INTENT(INOUT) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_GeneratePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                            GenerateCurves@GeneratreMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate curves

INTERFACE
  MODULE SUBROUTINE obj_GenerateCurves(obj, gmsh)
    CLASS(GmshStructuredMesh2_), INTENT(INOUT) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_GenerateCurves
END INTERFACE

!----------------------------------------------------------------------------
!                                           GenerateSurfaces@GenerateMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Generate Surfaces

INTERFACE
  MODULE SUBROUTINE obj_GenerateSurfaces(obj, gmsh)
    CLASS(GmshStructuredMesh2_), INTENT(INOUT) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_GenerateSurfaces
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(GmshStructuredMesh2_), INTENT(inout) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetMeshTypeName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-03-19
! summary: Get the mesh type name from integer code

INTERFACE
  MODULE FUNCTION obj_GetMeshTypeName(obj, meshType) RESULT(ans)
    CLASS(GmshStructuredMesh2_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetMeshTypeName
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeNumber@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetNodeNumber(obj, i, j) RESULT(ans)
    CLASS(GmshStructuredMesh2_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetEdgeNumberOnAxis1@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetEdgeNumberOnAxis1(obj, i, j) RESULT(ans)
    CLASS(GmshStructuredMesh2_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j
    !! point number
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetEdgeNumberOnAxis1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetEdgeNumberOnAxis2@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetEdgeNumberOnAxis2(obj, i, j) RESULT(ans)
    CLASS(GmshStructuredMesh2_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i, j
    !! point number
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetEdgeNumberOnAxis2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-20
! summary:         Import obj from toml table
!
!# ImportFromToml
!
! Import object from toml table.
!

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GmshStructuredMesh2_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data From toml file
!
!# ImportFromToml
!
! Initiate obj from toml file.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(GmshStructuredMesh2_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the key
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! txt file where toml config is stored
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! you can pass the filename, then we will make
    !! the file, open it and read the toml config and
    !! close the file.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! if it is true then we will print the toml config
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshStructuredMesh2_Class

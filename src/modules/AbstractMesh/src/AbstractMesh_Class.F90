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

MODULE AbstractMesh_Class
USE GlobalData, ONLY: LGT, I4B, DFP
USE HDF5File_Class, ONLY: HDF5File_
IMPLICIT NONE

PRIVATE
PUBLIC :: AbstractMesh_

CHARACTER(*), PARAMETER :: modName = "AbstractMesh_Class"

!----------------------------------------------------------------------------
!                                                             AbstractMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-26
! summary:  Abstract class for mesh

TYPE, ABSTRACT :: AbstractMesh_
  LOGICAL(LGT) :: readFromFile = .TRUE.
    !! True if the mesh is read from a file
  LOGICAL(LGT) :: isInitiated = .FALSE.
    !! logical flag denoting for whether mesh data is Initiated or not
  LOGICAL(LGT) :: isNodeToElementsInitiated = .FALSE.
    !! Node to elements mapping
  LOGICAL(LGT) :: isNodeToNodesInitiated = .FALSE.
    !! Node to nodes mapping
  LOGICAL(LGT) :: isExtraNodeToNodesInitiated = .FALSE.
    !! Node to nodes mapping
  LOGICAL(LGT) :: isElementToElementsInitiated = .FALSE.
    !! Element to elements mapping
  LOGICAL(LGT) :: isBoundaryDataInitiated = .FALSE.
    !! Boundary data
  LOGICAL(LGT) :: isFacetDataInitiated = .FALSE.
    !! FacetData
  INTEGER(I4B) :: uid = 0
    !! Unique id of the mesh
  INTEGER(I4B) :: tElements_topology_wise(8)
    !! point, line, triangle, quadrangle, tetrahedron, hexahedron, prism,
    !! pyramid
  INTEGER(I4B) :: tElemTopologies = 0, elemTopologies(8)
  !! total element topologies, name of element topologies are stored in
  !! elemTopologies(1:tElemTopologies)
  INTEGER(I4B) :: nsd = 0
    !! number of spatial dimension of the mesh
  INTEGER(I4B) :: maxNptrs = 0
    !! largest node number present inside the mesh
  INTEGER(I4B) :: minNptrs = 0
    !! minimum node number present inside the mesh
  INTEGER(I4B) :: maxElemNum = 0
    !! largest element number present inside the mesh
  INTEGER(I4B) :: minElemNum = 0
    !! minimum element number present inside the mesh
  INTEGER(I4B) :: tNodes = 0
    !! total number of nodes present inside the mesh
  INTEGER(I4B) :: tIntNodes = 0
    !! total number of internal nodes inside the mesh
  INTEGER(I4B) :: tElements = 0
    !! total number of elements present inside the mesh
    !! It is the size of elemNumber vector
  REAL(DFP) :: minX = 0.0
    !! minimum value of x coordinate
  REAL(DFP) :: maxX = 0.0
    !! maximum value of x coordinate
  REAL(DFP) :: minY = 0.0
    !! minimum value of y coordinate
  REAL(DFP) :: maxY = 0.0
    !! maximum value of y coordinate
  REAL(DFP) :: minZ = 0.0
    !! minimum value of z coordinate
  REAL(DFP) :: maxZ = 0.0
    !! maximum value of z coordinate
  REAL(DFP) :: x = 0.0
    !! x coorindate of centroid
  REAL(DFP) :: y = 0.0
    !! y coordinate of centroid
  REAL(DFP) :: z = 0.0
    !! z coordinate of centroid
  INTEGER(I4B), ALLOCATABLE :: physicalTag(:)
    !! Physical entities associated with the current entity (mesh)
    !! physical tags can be thought of as the material numbers
  INTEGER(I4B), ALLOCATABLE :: material(:)
    !! materials mapped to the mesh
    !! material(1) is the material id of medium 1
    !! material(2) is the material id of medium 2
    !! ...
    !! material(n) is the material id of medium n
    !!
    !! For example, soil is a porous medium n = 1,
    !! fluid is a medium n =2
    !! then material(1) denotes the type of soil => clay, sand, silt
    !! and material(2) denotes the type of fluid, water, oil, air
  INTEGER(I4B), ALLOCATABLE :: boundingEntity(:)
    !! Bounding entity numbers of the current entity
  INTEGER(I4B), ALLOCATABLE :: local_elemNumber(:)
    !! List of local element numbers, the lowerbound is `minElemNum`
    !! and upper bound is `maxElemNum`. In this way, local_elemNumber(iel)
    !! returns the local element number of global element number iel.

  INTEGER(I4B), ALLOCATABLE :: local_Nptrs(:)
    !! Returns local node number from a global node number
    !! Its length is from 1 to maxNptrs
    !! Helpul in finding if a global node is present inside the mesh or not

END TYPE AbstractMesh_

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Allocate the size of the mesh

ABSTRACT INTERFACE
  SUBROUTINE obj_Initiate(obj, hdf5, group)
    IMPORT :: HDF5File_, AbstractMesh_
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! Mesh file in hdf5 file format
    CHARACTER(*), INTENT(IN) :: group
    !! location in HDF5 file
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Free up the memory stored in [[obj_]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Deallocate(obj)
    IMPORT :: AbstractMesh_
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

END MODULE AbstractMesh_Class

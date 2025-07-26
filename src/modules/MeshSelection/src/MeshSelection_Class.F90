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

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This module defines a data type for mesh selection

MODULE MeshSelection_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: IntVector_, BoundingBox_
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE FPL, ONLY: ParameterList_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "MeshSelection_Class"
CHARACTER(*), PARAMETER :: myprefix = "MeshSelection"

PUBLIC :: MeshSelectionDeallocate
PUBLIC :: MeshSelection_
PUBLIC :: MeshSelectionPointer_
PUBLIC :: MeshSelectionImportParamFromToml
PUBLIC :: MeshSelectionImportFromToml
PUBLIC :: SetMeshSelectionParam
PUBLIC :: MeshSelectionSet

!----------------------------------------------------------------------------
!                                                            MeshSelection_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: Nov 7 2021
! summary: Datatype for selecting mesh (group of elements) in domain

TYPE :: MeshSelection_
  PRIVATE
  LOGICAL(LGT) :: isinit = .FALSE.
    !!  True if the instance is initiated
  LOGICAL(LGT) :: ms(4) = .FALSE.
  !! mesh selection
  !! index 1: selection by mesh id
  !! index 2: selection by element number
  !! index 3: selection by node number
  !! index 4: selection by box

  ! TODO: Convert all intvector into allocatable array
  ! and use expand operation instead of append

  INTEGER(I4B) :: totalPointMeshid = 0
  !! size of pointMeshid
  INTEGER(I4B) :: totalCurveMeshid = 0
  !! size of curveMeshid
  INTEGER(I4B) :: totalSurfaceMeshid = 0
  !! size of surfaceMeshid
  INTEGER(I4B) :: totalVolumeMeshid = 0
  !! size of volumeMeshid

  INTEGER(I4B) :: totalPointElemnum = 0
  !! size of pointElemnum
  INTEGER(I4B) :: totalCurveElemnum = 0
  !! size of curveElemnum
  INTEGER(I4B) :: totalSurfaceElemnum = 0
  !! size of surfaceElemnum
  INTEGER(I4B) :: totalVolumeElemnum = 0
  !! size of volumeElemnum

  INTEGER(I4B) :: totalPointNodenum = 0
  !! size of pointNodeNum
  INTEGER(I4B) :: totalCurveNodenum = 0
  !! size of curvePointNum
  INTEGER(I4B) :: totalSurfaceNodenum = 0
  !! size of surfacePointNum
  INTEGER(I4B) :: totalVolumeNodenum = 0
  !! size of volumePointNum

  TYPE(IntVector_) :: pointMeshID
    !! It denotes the IDs of mesh which has xidim = 0 (point-mesh)
  TYPE(IntVector_) :: curveMeshID
    !! It denotes the IDs of mesh which has xidim = 1 (curve-mesh)
  TYPE(IntVector_) :: surfaceMeshID
    !! It denotes the IDs of mesh which has xidim = 2 (surface-mesh)
  TYPE(IntVector_) :: volumeMeshID
    !! It denotes the IDs of mesh which has xidim = 3 (volume-mesh)

  TYPE(IntVector_) :: pointElemNum
    !! Element number in mesh of points
  TYPE(IntVector_) :: curveElemNum
    !! Element number in mesh of curves
  TYPE(IntVector_) :: surfaceElemNum
    !! Element number in mesh of surfaces
  TYPE(IntVector_) :: volumeElemNum
    !! Element number in mesh of volume

  !! INFO: Currently, we are not using this (futuristic)
  TYPE(IntVector_) :: pointNodeNum
    !! Global Node numbers in pointEntity
  TYPE(IntVector_) :: curveNodeNum
    !! Global Node numbers in cuveEntity
  TYPE(IntVector_) :: surfaceNodeNum
    !! Global Node numbers in surfaceEntity
  TYPE(IntVector_) :: volumeNodeNum
    !! Global Node numbers in volumeEntity
  TYPE(IntVector_) :: nodeNum
    !! Global Node numbers
  TYPE(BoundingBox_), ALLOCATABLE :: pointBox(:)
  !! boxes for point
  TYPE(BoundingBox_), ALLOCATABLE :: curveBox(:)
  !! boxes for line
  TYPE(BoundingBox_), ALLOCATABLE :: surfaceBox(:)
  !! boxes for surface
  TYPE(BoundingBox_), ALLOCATABLE :: volumeBox(:)
  !! boxes for volume

CONTAINS

  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! Check essential parameter

  PROCEDURE, PUBLIC, PASS(obj) :: isInitiated => obj_isInitiated

  PROCEDURE, PUBLIC, PASS(obj) :: isSelectionByMeshID => &
    obj_isSelectionByMeshID

  PROCEDURE, PUBLIC, PASS(obj) :: isSelectionByElemNum => &
    obj_isSelectionByElemNum

  PROCEDURE, PUBLIC, PASS(obj) :: isSelectionByNodeNum => &
    obj_isSelectionByNodeNum

  PROCEDURE, PUBLIC, PASS(obj) :: isSelectionByBox => &
    obj_isSelectionByBox

  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate an instance of meshSelection
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate an instance of meshSelection
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2
  !! Initiate an instance of meshSelection
  PROCEDURE, PASS(obj) :: Copy => obj_Copy
  !! This routine copies object
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Assignment operator
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate Data
  FINAL :: obj_Final

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Add => obj_Add
  !! Add a new region to the MeshSelection_
  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! This routine should be called when we are done
  !! setting the regions in the instance
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterialToMesh1 => obj_SetMaterialToMesh1
  !! Set material to mesh
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterialToMesh2 => obj_SetMaterialToMesh2
  !! Set materiall to mesh
  GENERIC, PUBLIC :: SetMaterialToMesh => &
    SetMaterialToMesh1, SetMaterialToMesh2
  !! Set material to mesh

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from the hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export to the HDF5File
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Displays the content
  PROCEDURE, PUBLIC, PASS(obj) :: ImportParamFromToml => &
    obj_ImportParamFromToml
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshID => obj_getMeshID
  !! Returns the mesh id if available

  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshIDPointer => obj_GetMeshIDPointer
  !! returns pointer to mesh id

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemNumPointer => obj_GetElemNumPointer
  !! returns pointer to mesh id

  ! GET:
  ! @GetElemNumMethods

  PROCEDURE, PASS(obj) :: GetTotalElemNum1 => obj_GetTotalElemNum1
  PROCEDURE, PASS(obj) :: GetTotalElemNum2 => obj_GetTotalElemNum2
  PROCEDURE, PASS(obj) :: GetTotalElemNum3 => obj_GetTotalElemNum3
  PROCEDURE, PASS(obj) :: GetTotalElemNum4 => obj_GetTotalElemNum4
  GENERIC, PUBLIC :: GetTotalElemNum => GetTotalElemNum1, GetTotalElemNum2, &
    GetTotalElemNum3, GetTotalElemNum4

  PROCEDURE, PASS(obj) :: GetElemNum1 => obj_GetElemNum1
  !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: GetElemNum2 => obj_GetElemNum2
  !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: GetElemNum3 => obj_GetElemNum3
  !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: GetElemNum4 => obj_GetElemNum4
  !! Returns the element numbers if available
  GENERIC, PUBLIC :: GetElemNum => GetElemNum1, GetElemNum2, &
    GetElemNum3, GetElemNum4
  !! Returns the element numbers if available

  PROCEDURE, PASS(obj) :: GetTotalNodeNum1 => obj_GetTotalNodeNum1
  PROCEDURE, PASS(obj) :: GetTotalNodeNum2 => obj_GetTotalNodeNum2
  PROCEDURE, PASS(obj) :: GetTotalNodeNum3 => obj_GetTotalNodeNum3
  GENERIC, PUBLIC :: GetTotalNodeNum => GetTotalNodeNum1, GetTotalNodeNum2, &
    GetTotalNodeNum3

  PROCEDURE, PASS(obj) :: obj_GetNodeNum1
  PROCEDURE, PASS(obj) :: obj_GetNodeNum2
  PROCEDURE, PASS(obj) :: obj_GetNodeNum3
  GENERIC, PUBLIC :: GetNodeNum => obj_GetNodeNum1, obj_GetNodeNum2, &
    obj_GetNodeNum3
    !! Returns the node number if available

  PROCEDURE, PUBLIC, PASS(obj) :: IsMeshIDAllocated => &
    obj_IsMeshIDAllocated
    !! returns true if selection by meshID is allocated
  PROCEDURE, PUBLIC, PASS(obj) :: isElemNumAllocated => &
    obj_isElemNumAllocated
    !! returns true if element numbers are allocated
  PROCEDURE, PUBLIC, PASS(obj) :: isNodeNumAllocated => &
    obj_isNodeNumAllocated
    !! returns true if the node numbers are allocated
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
    !! Query the mesh selection
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
END TYPE MeshSelection_

!----------------------------------------------------------------------------
!                                                            MeshSelection_
!----------------------------------------------------------------------------

TYPE(MeshSelection_), PUBLIC, PARAMETER ::TypeMeshSelection=MeshSelection_()

!----------------------------------------------------------------------------
!                                                     MeshSelectionPointer_
!----------------------------------------------------------------------------

TYPE :: MeshSelectionPointer_
  CLASS(MeshSelection_), POINTER :: ptr => NULL()
END TYPE MeshSelectionPointer_

!----------------------------------------------------------------------------
!                                             isInitiated@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_isInitiated(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                    isSelectionByMeshID@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_isSelectionByMeshID(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isSelectionByMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                    isSelectionByElemNum@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_isSelectionByElemNum(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isSelectionByElemNum
END INTERFACE

!----------------------------------------------------------------------------
!                                   isSelectionByNodeNum@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_isSelectionByNodeNum(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isSelectionByNodeNum
END INTERFACE

!----------------------------------------------------------------------------
!                                       isSelectionByBox@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_isSelectionByBox(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isSelectionByBox
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE MeshSelectionDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(MeshSelection_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE MeshSelectionDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE MeshSelectionDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(MeshSelectionPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE MeshSelectionDeallocate

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Check essential parameter

INTERFACE
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param, prefix)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                   SetMeshSelectionParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Set mesh parameters

INTERFACE
  MODULE SUBROUTINE SetMeshSelectionParam(param, prefix, &
                isSelectionByMeshID, isSelectionByElemNum, isSelectionByBox, &
                                          isSelectionByNodeNum)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByMeshID
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByElemNum
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByBox
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByNodeNum
  END SUBROUTINE SetMeshSelectionParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: Initiate an instance of meshSelection (Deprecated)

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, isSelectionByMeshID, &
                                  isSelectionByElemNum, &
                                  isSelectionByBox, &
                                  isSelectionByNodeNum)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByMeshID
    !! Is selection by MeshID
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByElemNum
    !! is selection by element num
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByBox
    !! is selection by bounding box
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByNodeNum
    !! is selection by node number
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: Initiate an instance of MeshSelection_

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, param)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sep 2021
! summary: Initiate an instance of MeshSelection_ by copying other object

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    CLASS(MeshSelection_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                        Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine deallocates data stored inside [[MeshSelection_]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This is finalizer for instance of meshSelection

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(MeshSelection_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine adds data to the meshSelection
!
!# Introduction
!
! Task-1: Internally it is performed by addmeshid
!         If dim and meshID are provides then it will append meshID to the
!         pointMeshID, curveMeshID, surfaceMeshID, or volumeMeshID
!         depending on the value of dim.
!
! Task-2: Internally it is performed by addelemnum
!         If dim and elemNum are provided then it will append elemNum to the
!         pointElemNum, curveElemNum, surfaceElemNum, or volumeElemNum
!         depending on the value of dim.
!
! Task-3: Internally it is performed by addnodenum
!         If nodeNum and dim are provided then it will append nodeNum to
!         pointNodeNum, curveNodeNum, surfaceNodeNum, or volumeNodeNum
!         depending on the value of dim.
!         If dim is not provided then it will append nodeNum to nodeNum
!
! Task-4: Internally it is performed by addbox
!         If dim and box are provided then it will append box to the
!         pointBox, curveBox, surfaceBox, or volumeBox
!         depending on the value of dim.

INTERFACE
  MODULE SUBROUTINE obj_Add(obj, dom, dim, meshID, box, elemNum, &
                            nodeNum)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension of the mesh region
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshID(:)
    !! List of mesh IDss
    TYPE(BoundingBox_), OPTIONAL, INTENT(IN) :: box(:)
    !! bounding boxes
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemNum(:)
    !! element numbers
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nodeNum(:)
    !! node numbers
  END SUBROUTINE obj_Add
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine adds data to the meshSelection

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine adds data to the meshSelection
!
!# Introduction
!   This routine calls Set Method for each obj

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Set2
END INTERFACE

INTERFACE MeshSelectionSet
  MODULE PROCEDURE obj_Set2
END INTERFACE MeshSelectionSet

!----------------------------------------------------------------------------
!                                               SetMaterialToMesh@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-29
! summary:  Set material to mesh
!
!# Introduction
!
! This routine set the material to the mesh. It performs following tasks
!
! Task-1: isSelectionByMeshID
! - First it get the pointer to mesh from dom of given dim
! - Then it gets the meshID from obj for the given dim
! - Then for each meshID it calls
!   mesh%SetMaterial(entityNum, material, medium)
!
! Task-2: isSelectionByElemNum
! - In this case it gets pointer to elemNum for given dim
!   Then for each element number it calls
!   mesh%SetMaterial(medium, material, globalElement, islocal)
!
! Following tasks are pending
!
! Task-3: isSelectionByNodeNum
! Task-4: isSelectionByBox

INTERFACE
  MODULE SUBROUTINE obj_SetMaterialToMesh1(obj, dom, dim, medium, material)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B), INTENT(IN) :: material
  END SUBROUTINE obj_SetMaterialToMesh1
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetMaterialToMesh@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-29
! summary:  Set material to mesh
!
!# Introduction
!   This method calls SetMaterialToMesh1 for each dimension

INTERFACE
  MODULE SUBROUTINE obj_SetMaterialToMesh2(obj, dom, medium, material)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B), INTENT(IN) :: material
  END SUBROUTINE obj_SetMaterialToMesh2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by import

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                              ImportParamFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param by reading the toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportParamFromToml(obj, param, table)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportParamFromToml
END INTERFACE

INTERFACE MeshSelectionImportParamFromToml
  MODULE PROCEDURE obj_ImportParamFromToml
END INTERFACE MeshSelectionImportParamFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE MeshSelectionImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE MeshSelectionImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE MeshSelectionImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE MeshSelectionImportFromToml

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by Export

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by Export

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMeshID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION obj_GetMeshID(obj, dim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMeshIDPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-29
! summary:  Get mesh id pointer
!
!# Introduction
!
! Get pointer to pointMeshID, curveMeshID, surfaceMeshID, and volumeMeshID
! for dim = 0, 1, 2, and 3 respectively

INTERFACE
  MODULE SUBROUTINE obj_GetMeshIDPointer(obj, dim, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), POINTER, INTENT(OUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetMeshIDPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemNumPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-29
! summary:  Get element number pointer
!
!# Introduction
!
! Get pointer to pointElemnum, curveElemnum, surfaceElemNum,
! and volumeElemnum for dim = 0, 1, 2, and 3 respectively

INTERFACE
  MODULE SUBROUTINE obj_GetElemNumPointer(obj, dim, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), POINTER, INTENT(OUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetElemNumPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                              IsMeshIDAllocated@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns true if meshID of given dim is allocated

INTERFACE
  MODULE PURE FUNCTION obj_IsMeshIDAllocated(obj, dim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsMeshIDAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                            isElemNumAllocated@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION obj_IsElemNumAllocated(obj, dim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsElemNumAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                             isNodeNumAllocated@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns true if node numbers are allocated

INTERFACE
  MODULE PURE FUNCTION obj_IsNodeNumAllocated(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsNodeNumAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetParam@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE obj_GetParam(obj, isInitiated, isSelectionByBox, &
              isSelectionByMeshID, isSelectionByElemNum, isSelectionByNodeNum)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByBox
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByMeshID
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByElemNum
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByNodeNum
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-17
! summary:  Get total element number
!
!# Introduction
!
! Get total number of elements in the mesh selection
! This method works when isSelectionByElemNum is true
!
! For dim=0 it returns the size of pointElemNum
! For dim=1 it returns the size of curveElemNum
! For dim=2 it returns the size of surfaceElemNum
! For dim=3 it returns the size of volumeElemNum

INTERFACE
  MODULE FUNCTION obj_GetTotalElemNum1(obj, dim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElemNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-17
! summary:  Get total element number
!
!# Introduction
!
! Get total number of elements in the mesh selection
! Note: This method works when isSelectionByMeshID is true
!
! For dim=0, it returns size of pointElemNum
! For dim=1, it returns size of curveElemNum
! For dim=2, it returns size of surfaceElemNum
! For dim=3, it returns size of volumeElemNum

INTERFACE
  MODULE FUNCTION obj_GetTotalElemNum2(obj, dim, dom) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElemNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-17
! summary:  Get total element number
!
!# Introduction
!
! Get total number of elements in the mesh selection
!
! This method returns the sum of size of pointElemNum, curveElemNum,
! surfaceElemNum, and volumeElemNum

INTERFACE
  MODULE FUNCTION obj_GetTotalElemNum3(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElemNum3
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-29
! summary:  Get total element number
!
!# Introduction
!
! Get total number of elements in the mesh selection
! This method returns the total number of elements in
! pointMeshID, curveMeshID, surfaceMeshID, and volumeMeshID

INTERFACE
  MODULE FUNCTION obj_GetTotalElemNum4(obj, dom) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElemNum4
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: Returns element number if isSelectionByElemNum is true
!
!# Introduction
!
! Get element number if isSelectionByElemNum is true
!
! dim = 0, returns pointElemNum
! dim = 1, returns curveElemNum
! dim = 2, returns surfaceElemNum
! dim = 3, returns volumeElemNum

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum1(obj, dim, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim =0, pointElemnum
    !! dim =1, curveElemnum
    !! dim =2, surfaceElemnum
    !! dim =3, volumeElemnum
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! element number
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of element number
  END SUBROUTINE obj_GetElemNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Nov 2021
! summary: Returns element number
!
!
!# Introduction
!
! This routine returns the element number by using dimension.
! It works when
!
! - [x] isSelectionByMeshID
! - [x] isSelectionByElemNum
! - [  ] isSelectionByNodeNum
! - [  ] isSelectionByBox

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum2(obj, dim, dom, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension
    !! dim=0, pointElemNum
    !! dim=1, curveElemNum
    !! dim=2, surfaceElemNum
    !! dim=3, volumeElemNum
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! abstract domain
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! element number
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of element number
  END SUBROUTINE obj_GetElemNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: Returns element number if isSelectionByElemNum is true
!
!# Introduction
!
! Get element number for all dimension
!
! this routine works when isSelectionByElemNum is true
!
! we get element number from
! - pointElemNum
! - curveElemNum
! - surfaceElemNum
! - volumeElemNum

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum3(obj, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetElemNum3
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Nov 2021
! summary: Returns element number
!
!
!# Introduction
!
! This function returns the element number from all dimension.
! It works when
!
! - [x] isSelectionByMeshID
! - [x] isSelectionByElemNum
! - [  ] isSelectionByNodeNum
! - [  ] isSelectionByBox
!
! this routine calls obj_GetElemNum2 for dim=0,1,2,3

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum4(obj, dom, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetElemNum4
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalNodenum@GetNodeNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2024-07-17
! summary:  Get total node number

INTERFACE
  MODULE FUNCTION obj_GetTotalNodenum1(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodenum1
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalNodenum@GetNodeNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-17
! summary:  Get total nodes of a meshid

INTERFACE
  MODULE FUNCTION obj_GetTotalNodenum2(obj, dim, dom, onlydim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim is used only to access the mesh from domain
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlydim
    !! only for development, so that we can call it from gettotalnodenum3
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodenum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns node numbers

INTERFACE
  MODULE FUNCTION obj_GetTotalNodenum3(obj, dom) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodenum3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns the node numbers from nodenum

INTERFACE
  MODULE SUBROUTINE obj_GetNodeNum1(obj, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetNodeNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns node numbers from nodenum, meshid, elemnum
!
! - [x] isSelectionByMeshID
! - [x] isSelectionByElemNum
! - [x] isSelectionByNodeNum
! - [  ] isSelectionByBox

INTERFACE
  MODULE SUBROUTINE obj_GetNodeNum2(obj, dim, dom, ans, tsize, onlydim)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlydim
    !! only for development so that we can call it from getnodenum3
  END SUBROUTINE obj_GetNodeNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns node numbers

INTERFACE
  MODULE SUBROUTINE obj_GetNodeNum3(obj, dom, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetNodeNum3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MeshSelection_Class

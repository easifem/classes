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
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE HDF5File_Class, ONLY: HDF5File_
USE Domain_Class, ONLY: Domain_
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "MESHSELECTION_CLASS"
TYPE(ExceptionHandler_) :: e

!----------------------------------------------------------------------------
!                                                            MeshSelection_
!----------------------------------------------------------------------------
!> authors: Vikas Sharma, Ph. D.
! date: Nov 7 2021
! summary: Datatype for selecting mesh (group of elements) in domain
!
!{!pages/MeshSelection_.md!}

TYPE :: MeshSelection_
  PRIVATE
  LOGICAL(LGT), PUBLIC :: isInitiated = .FALSE.
    !!  True if the instance is initiated
  LOGICAL(LGT), PUBLIC :: isSelectionByMeshID = .FALSE.
    !! True if selection by mesh id
  LOGICAL(LGT), PUBLIC :: isSelectionByElemNum = .FALSE.
    !! True if selection by element number
  LOGICAL(LGT), PUBLIC :: isSelectionByBox = .FALSE.
    !! True if selection by box
  LOGICAL(LGT), PUBLIC :: isSelectionByNodeNum = .FALSE.
    !! True if selection by node number
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
  TYPE(IntVector_) :: nodeNum
    !! Global Node numbers
  !! TODO add BoundingBox to MeshSelection_
  !! type(BoundingBoxPointer_), allocatable :: bbox(:)
  !! Accordingly, modify the initiate method.
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: addSurrogate => meshSelect_addSurrogate
    !! add surrogates to the module's [[ExceptionHandler_]]
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => meshSelect_Initiate
    !! Initiates an instance of [[MeshSelection_]]
  PROCEDURE, PASS(obj) :: Copy => meshSelect_Copy
    !! This routine copies object
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
    !! Assignment operator
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => &
    & meshSelect_DeallocateData
    !! Deallocate Data
  FINAL :: meshSelect_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Add => meshSelect_Add
    !! Add a new region to mesh selection
  PROCEDURE, PUBLIC, PASS(obj) :: Set => meshSelect_Set
    !! This routine should be called when we are done
    !! setting the regions in the instance
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => meshSelect_Import
    !! Import from the hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => meshSelect_Export
    !! Export to the HDF5File
  PROCEDURE, PUBLIC, PASS(obj) :: Display => meshSelect_Display
    !! Displays the content
  PROCEDURE, PUBLIC, PASS(obj) :: getMeshID => meshSelect_getMeshID
    !! Returns the mesh id if available
  PROCEDURE, PASS(obj) :: meshSelect_getElemNum1
    !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: meshSelect_getElemNum2
    !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: meshSelect_getElemNum3
    !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: meshSelect_getElemNum4
    !! Returns the element numbers if available
  GENERIC, PUBLIC :: getElemNum => &
       & meshSelect_getElemNum1, &
       & meshSelect_getElemNum2, &
       & meshSelect_getElemNum3, &
       & meshSelect_getElemNum4
    !! Returns the element numbers if available
  PROCEDURE, PUBLIC, PASS(obj) :: getNodeNum => meshSelect_getNodeNum
    !! Returns the node number if available
  PROCEDURE, PUBLIC, PASS(obj) :: isMeshIDAllocated => &
    & meshSelect_isMeshIDAllocated
    !! returns true if selection by meshID is allocated
  PROCEDURE, PUBLIC, PASS(obj) :: isElemNumAllocated => &
    & meshSelect_isElemNumAllocated
    !! returns true if element numbers are allocated
  PROCEDURE, PUBLIC, PASS(obj) :: isNodeNumAllocated => &
    & meshSelect_isNodeNumAllocated
    !! returns true if the node numbers are allocated
END TYPE MeshSelection_

PUBLIC :: MeshSelection_

!----------------------------------------------------------------------------
!                                                            MeshSelection_
!----------------------------------------------------------------------------

TYPE(MeshSelection_), PUBLIC, PARAMETER :: TypeMeshSelection = &
  & MeshSelection_()

!----------------------------------------------------------------------------
!                                                     MeshSelectionPointer_
!----------------------------------------------------------------------------

TYPE :: MeshSelectionPointer_
  CLASS(MeshSelection_), POINTER :: ptr => NULL()
END TYPE MeshSelectionPointer_

PUBLIC :: MeshSelectionPointer_

!----------------------------------------------------------------------------
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: Adds surrogate to the module exceptionHandler_

INTERFACE
  MODULE SUBROUTINE meshSelect_addSurrogate(obj, UserObj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(ExceptionHandler_), INTENT(IN) :: UserObj
  END SUBROUTINE meshSelect_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: Initiate an instance of [[MeshSelection_]]

INTERFACE
  MODULE SUBROUTINE meshSelect_Initiate(obj, isSelectionByMeshID, &
    & isSelectionByElemNum, isSelectionByBox, isSelectionByNodeNum)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByMeshID
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByElemNum
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByBox
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByNodeNum
  END SUBROUTINE meshSelect_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sep 2021
! summary: Initiate an instance of [[MeshSelection_]] by copying other object

INTERFACE
  MODULE SUBROUTINE meshSelect_Copy(obj, obj2)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    CLASS(MeshSelection_), INTENT(IN) :: obj2
  END SUBROUTINE meshSelect_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                        DeallocateData@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine deallocates data stored inside [[MeshSelection_]]

INTERFACE
  MODULE SUBROUTINE meshSelect_DeallocateData(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
  END SUBROUTINE meshSelect_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This is finalizer for instance of meshSelection

INTERFACE
  MODULE SUBROUTINE meshSelect_Final(obj)
    TYPE(MeshSelection_), INTENT(INOUT) :: obj
  END SUBROUTINE meshSelect_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine adds data to the meshSelection

INTERFACE
  MODULE SUBROUTINE meshSelect_Add(obj, dom, dim, meshID, box, elemNum, &
    & nodeNum)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(Domain_), OPTIONAL, INTENT(IN) :: dom
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshID(:)
    TYPE(BoundingBox_), OPTIONAL, INTENT(IN) :: box
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemNum(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nodeNum(:)
  END SUBROUTINE meshSelect_Add
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine adds data to the meshSelection

INTERFACE
  MODULE SUBROUTINE meshSelect_Set(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
  END SUBROUTINE meshSelect_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by import

INTERFACE
  MODULE SUBROUTINE meshSelect_Import(obj, hdf5, group, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
    TYPE(Domain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE meshSelect_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by Export

INTERFACE
  MODULE SUBROUTINE meshSelect_Export(obj, hdf5, group)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
  END SUBROUTINE meshSelect_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine initiate the object by Export

INTERFACE
  MODULE SUBROUTINE meshSelect_Display(obj, msg, unitNo)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE meshSelect_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getMeshID@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION meshSelect_getMeshID(obj, dim) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_getMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getElemNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: Returns element number if isSelectionByElemNum is true

INTERFACE
  MODULE FUNCTION meshSelect_getElemNum1(obj, dim) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_getElemNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getElemNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Nov 2021
! summary: Returns element number
!
!
!# Introduction
!
! This function returns the element number. It works when
!
! - [x] isSelectionByMeshID
! - [x] isSelectionByElemNum
! - [  ] isSelectionByNodeNum
! - [  ] isSelectionByBox

INTERFACE
  MODULE FUNCTION meshSelect_getElemNum2(obj, dim, domain) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    CLASS(Domain_), INTENT(IN) :: domain
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_getElemNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getElemNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: Returns element number if isSelectionByElemNum is true

INTERFACE
  MODULE FUNCTION meshSelect_getElemNum3(obj) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_getElemNum3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getElemNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Nov 2021
! summary: Returns element number
!
!
!# Introduction
!
! This function returns the element number. It works when
!
! - [x] isSelectionByMeshID
! - [x] isSelectionByElemNum
! - [  ] isSelectionByNodeNum
! - [  ] isSelectionByBox

INTERFACE
  MODULE FUNCTION meshSelect_getElemNum4(obj, domain) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CLASS(Domain_), INTENT(IN) :: domain
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_getElemNum4
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getNodeNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION meshSelect_getNodeNum(obj) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_getNodeNum
END INTERFACE

!----------------------------------------------------------------------------
!                                              isMeshIDAllocated@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION meshSelect_isMeshIDAllocated(obj, dim) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    LOGICAL(LGT) :: ans
  END FUNCTION meshSelect_isMeshIDAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                            isElemNumAllocated@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION meshSelect_isElemNumAllocated(obj, dim) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    LOGICAL(LGT) :: ans
  END FUNCTION meshSelect_isElemNumAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                             isNodeNumAllocated@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION meshSelect_isNodeNumAllocated(obj) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION meshSelect_isNodeNumAllocated
END INTERFACE

END MODULE MeshSelection_Class

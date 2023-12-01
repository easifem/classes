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
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE Domain_Class, ONLY: Domain_
USE FPL, ONLY: ParameterList_
USE tomlf, ONLY: toml_table
USE TxtFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "MeshSelection_Class"
CHARACTER(*), PARAMETER :: myprefix = "MeshSelection"
PUBLIC :: DEALLOCATE
PUBLIC :: MeshSelection_
PUBLIC :: MeshSelectionPointer_
PUBLIC :: MeshSelectionImportParamFromToml
PUBLIC :: MeshSelectionImportFromToml
PUBLIC :: SetMeshSelectionParam

!----------------------------------------------------------------------------
!                                                            MeshSelection_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: Nov 7 2021
! summary: Datatype for selecting mesh (group of elements) in domain

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
  TYPE(IntVector_) :: pointNodeNum
    !! Global Node numbers in pointEntity
    !! INFO: Currently, we are not using this (futuristic) 
  TYPE(IntVector_) :: curveNodeNum
    !! Global Node numbers in cuveEntity
    !! INFO: Currently, we are not using this (futuristic) 
  TYPE(IntVector_) :: surfaceNodeNum
    !! Global Node numbers in surfaceEntity
    !! INFO: Currently, we are not using this (futuristic) 
  TYPE(IntVector_) :: volumeNodeNum
    !! Global Node numbers in volumeEntity
    !! INFO: Currently, we are not using this (futuristic) 
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
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam =>  &
    & meshSelect_CheckEssentialParam
    !! Check essential parameter
  PROCEDURE, PASS(obj) :: Initiate1 => meshSelect_Initiate1
    !! Initiate an instance of meshSelection
  PROCEDURE, PASS(obj) :: Initiate2 => meshSelect_Initiate2
    !! Initiate an instance of meshSelection
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2
    !! Initiate an instance of meshSelection
  PROCEDURE, PASS(obj) :: Copy => meshSelect_Copy
    !! This routine copies object
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
    !! Assignment operator
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => meshSelect_Deallocate
    !! Deallocate Data
  FINAL :: meshSelect_Final

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Add => meshSelect_Add
    !! Add a new region to the MeshSelection_
  PROCEDURE, PUBLIC, PASS(obj) :: Set => meshSelect_Set
    !! This routine should be called when we are done
    !! setting the regions in the instance

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => meshSelect_Import
    !! Import from the hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => meshSelect_Export
    !! Export to the HDF5File
  PROCEDURE, PUBLIC, PASS(obj) :: Display => meshSelect_Display
    !! Displays the content
  PROCEDURE, PUBLIC, PASS(obj) :: ImportParamFromToml =>  &
    & meshSelect_ImportParamFromToml
  PROCEDURE, PASS(obj) :: ImportFromToml1 =>  &
    & meshSelect_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 =>  &
    & meshSelect_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml =>  &
    & ImportFromToml1, ImportFromToml2

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshID => meshSelect_getMeshID
    !! Returns the mesh id if available
  PROCEDURE, PASS(obj) :: meshSelect_GetElemNum1

    !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: meshSelect_GetElemNum2
    !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: meshSelect_GetElemNum3
    !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: meshSelect_GetElemNum4
    !! Returns the element numbers if available
  GENERIC, PUBLIC :: GetElemNum => &
    & meshSelect_GetElemNum1, &
    & meshSelect_GetElemNum2, &
    & meshSelect_GetElemNum3, &
    & meshSelect_GetElemNum4
    !! Returns the element numbers if available
  PROCEDURE, PASS(obj) :: meshSelect_GetNodeNum1
  PROCEDURE, PASS(obj) :: meshSelect_GetNodeNum2
  PROCEDURE, PASS(obj) :: meshSelect_GetNodeNum3
  GENERIC, PUBLIC :: GetNodeNum => &
    & meshSelect_GetNodeNum1, &
    & meshSelect_GetNodeNum2, &
    & meshSelect_GetNodeNum3
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
  PROCEDURE, PUBLIC, PASS(obj) :: GetQuery => meshSelect_GetQuery
    !! Query the mesh selection
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => meshSelect_GetQuery
    !! Query the mesh selection
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => meshSelect_GetPrefix
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
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(MeshSelection_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(MeshSelectionPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Check essential parameter

INTERFACE
  MODULE SUBROUTINE meshSelect_CheckEssentialParam(obj, param, prefix)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE meshSelect_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                   SetMeshSelectionParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Set mesh parameters

INTERFACE
  MODULE SUBROUTINE SetMeshSelectionParam(param,  &
    & prefix, &
    & isSelectionByMeshID, &
    & isSelectionByElemNum,  &
    & isSelectionByBox,  &
    & isSelectionByNodeNum)
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
  MODULE SUBROUTINE meshSelect_Initiate1( &
    & obj, &
    & isSelectionByMeshID, &
    & isSelectionByElemNum, &
    & isSelectionByBox, &
    & isSelectionByNodeNum)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByMeshID
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByElemNum
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByBox
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByNodeNum
  END SUBROUTINE meshSelect_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: Initiate an instance of MeshSelection_

INTERFACE
  MODULE SUBROUTINE meshSelect_Initiate2(obj, param)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
  END SUBROUTINE meshSelect_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sep 2021
! summary: Initiate an instance of MeshSelection_ by copying other object

INTERFACE
  MODULE SUBROUTINE meshSelect_Copy(obj, obj2)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    CLASS(MeshSelection_), INTENT(IN) :: obj2
  END SUBROUTINE meshSelect_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                        Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This routine deallocates data stored inside [[MeshSelection_]]

INTERFACE
  MODULE SUBROUTINE meshSelect_Deallocate(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
  END SUBROUTINE meshSelect_Deallocate
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
    TYPE(BoundingBox_), OPTIONAL, INTENT(IN) :: box(:)
    !! boxes
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
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE meshSelect_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                              ImportParamFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param by reading the toml table

INTERFACE MeshSelectionImportParamFromToml
  MODULE SUBROUTINE meshSelect_ImportParamFromToml(obj, param, table)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE meshSelect_ImportParamFromToml
END INTERFACE MeshSelectionImportParamFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE MeshSelectionImportFromToml
  MODULE SUBROUTINE meshSelect_ImportFromToml1(obj, table, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(Domain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE meshSelect_ImportFromToml1
END INTERFACE MeshSelectionImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE MeshSelectionImportFromToml
  MODULE SUBROUTINE meshSelect_ImportFromToml2(obj, tomlName, afile,  &
      & filename, printToml, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    TYPE(Domain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE meshSelect_ImportFromToml2
END INTERFACE MeshSelectionImportFromToml

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
    CHARACTER(*), INTENT(IN) :: group
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
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE meshSelect_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMeshID@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION meshSelect_GetMeshID(obj, dim) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_GetMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                              isMeshIDAllocated@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns true if meshID of given dim is allocated

INTERFACE
  MODULE PURE FUNCTION meshSelect_IsMeshIDAllocated(obj, dim) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    LOGICAL(LGT) :: ans
  END FUNCTION meshSelect_IsMeshIDAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                            isElemNumAllocated@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns MeshID

INTERFACE
  MODULE PURE FUNCTION meshSelect_IsElemNumAllocated(obj, dim) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    LOGICAL(LGT) :: ans
  END FUNCTION meshSelect_IsElemNumAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                             isNodeNumAllocated@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns true if node numbers are allocated

INTERFACE
  MODULE PURE FUNCTION meshSelect_IsNodeNumAllocated(obj) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION meshSelect_IsNodeNumAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetQuery@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE meshSelect_GetQuery( &
    & obj, &
    & isInitiated, &
    & isSelectionByBox, &
    & isSelectionByMeshID, &
    & isSelectionByElemNum, &
    & isSelectionByNodeNum)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByBox
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByMeshID
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByElemNum
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByNodeNum
  END SUBROUTINE meshSelect_GetQuery
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetElemNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: Returns element number if isSelectionByElemNum is true

INTERFACE
  MODULE FUNCTION meshSelect_GetElemNum1(obj, dim) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_GetElemNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetElemNum@getMethods
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
  MODULE FUNCTION meshSelect_GetElemNum2(obj, dim, domain) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    CLASS(Domain_), INTENT(IN) :: domain
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_GetElemNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetElemNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: Returns element number if isSelectionByElemNum is true

INTERFACE
  MODULE FUNCTION meshSelect_GetElemNum3(obj) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_GetElemNum3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetElemNum@getMethods
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
  MODULE FUNCTION meshSelect_GetElemNum4(obj, domain) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CLASS(Domain_), INTENT(IN) :: domain
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_GetElemNum4
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns the node numbers

INTERFACE
  MODULE FUNCTION meshSelect_GetNodeNum1(obj) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_GetNodeNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns node numbers
!
! - [x] isSelectionByMeshID
! - [x] isSelectionByElemNum
! - [x] isSelectionByNodeNum
! - [  ] isSelectionByBox

INTERFACE
  MODULE FUNCTION meshSelect_GetNodeNum2(obj, dim, domain) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    CLASS(Domain_), INTENT(IN) :: domain
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_GetNodeNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 Aug 2021
! summary: This routine returns node numbers

INTERFACE
  MODULE FUNCTION meshSelect_GetNodeNum3(obj, domain) RESULT(Ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CLASS(Domain_), INTENT(IN) :: domain
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION meshSelect_GetNodeNum3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION meshSelect_GetPrefix(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION meshSelect_GetPrefix
END INTERFACE

END MODULE MeshSelection_Class

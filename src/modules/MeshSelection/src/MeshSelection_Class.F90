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
! You should have received a copy of the GNU General Publi License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: This module defines a data type for mesh selection
!
! MeshSelection_ holds a group of mesh entities, elements or nodes of a
! domain. Boundary conditions and materials use it to select their region.
!
! Changes:
! - 2026-11-06: Documented every method and added tests in examples/

MODULE MeshSelection_Class
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE BaseType, ONLY: BoundingBox_
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE String_Class, ONLY: String
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE
PRIVATE

PUBLIC :: MeshSelectionDeallocate
PUBLIC :: MeshSelectionReallocate
PUBLIC :: MeshSelection_
PUBLIC :: MeshSelectionPointer_
PUBLIC :: MeshSelectionSet
PUBLIC :: MeshSelectionDisplay
PUBLIC :: TypeMeshSelection

!----------------------------------------------------------------------------
!                                                            MeshSelection_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-07
! summary: Datatype for selecting mesh (group of elements) in domain
!
! A selection can be made by mesh ID, element number, node number and
! bounding box, and these can be used together. Each of them has one list
! for each dimension (point, curve, surface and volume). Global node
! numbers can also be given without a dimension, in nodeNum.
!
! The usual order of calls is Initiate, then Add for each list, then Set.
! The Get methods with a dom argument turn mesh IDs into elements or nodes.
!
! Examples
!
!```fortran
! TYPE(MeshSelection_) :: obj
! CALL obj%Initiate()
! CALL obj%Add(dim=2, meshID=[2])
! CALL obj%Add(dim=1, elemNum=[15])
! CALL obj%Set()
! tsize = obj%GetTotalElemNum(dom=dom)
!```
!
! Full example: src/modules/MeshSelection/examples/GetElemNum_test_1.F90

TYPE :: MeshSelection_
  PRIVATE
  LOGICAL(LGT) :: isinit = .FALSE.
  !! True if the instance is initiated
  LOGICAL(LGT) :: ms(4) = .FALSE.
  !! mesh selection
  !! index 1: selection by mesh id
  !! index 2: selection by element number
  !! index 3: selection by node number
  !! index 4: selection by box
  ! TODO: Convert all intvector into allocatable array
  ! and use expand operation instead of append
  INTEGER(I4B) :: totalPointMeshid = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalCurveMeshid = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalSurfaceMeshid = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalVolumeMeshid = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalPointElemnum = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalCurveElemnum = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalSurfaceElemnum = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalVolumeElemnum = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalPointNodenum = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalCurveNodenum = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalSurfaceNodenum = 0
  !! Not used, Add does not update it
  INTEGER(I4B) :: totalVolumeNodenum = 0
  !! Not used, Add does not update it
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
    !! Global node numbers of dim 0, Set appends them to nodeNum
  TYPE(IntVector_) :: curveNodeNum
    !! Global node numbers of dim 1, Set appends them to nodeNum
  TYPE(IntVector_) :: surfaceNodeNum
    !! Global node numbers of dim 2, Set appends them to nodeNum
  TYPE(IntVector_) :: volumeNodeNum
    !! Global node numbers of dim 3, Set appends them to nodeNum
  TYPE(IntVector_) :: nodeNum
    !! Global node numbers of the selection, sorted and unique after Set
  TYPE(BoundingBox_), ALLOCATABLE :: pointBox(:)
  !! boxes for point
  TYPE(BoundingBox_), ALLOCATABLE :: curveBox(:)
  !! boxes for line
  TYPE(BoundingBox_), ALLOCATABLE :: surfaceBox(:)
  !! boxes for surface
  TYPE(BoundingBox_), ALLOCATABLE :: volumeBox(:)
  !! boxes for volume

CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Returns true if the object Is Initiated.
  PROCEDURE, PUBLIC, PASS(obj) :: IsSelectionByMeshID => &
    obj_IsSelectionByMeshID
  !! Returns Is selection by mesh id.
  PROCEDURE, PUBLIC, PASS(obj) :: IsSelectionByElemNum => &
    obj_IsSelectionByElemNum
  !! Returns true if selection by element number Is true.
  PROCEDURE, PUBLIC, PASS(obj) :: IsSelectionByNodeNum => &
    obj_IsSelectionByNodeNum
  !! Returns true if selection by node number Is set to true.
  PROCEDURE, PUBLIC, PASS(obj) :: IsSelectionByBox => &
    obj_IsSelectionByBox
  !! Returns true if selection by box is set to true.
  PROCEDURE, PRIVATE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate an instance of meshSelection
  GENERIC, PUBLIC :: Initiate => Initiate1
  !! Initiate an instance of meshSelection
  PROCEDURE, PRIVATE, PASS(obj) :: Copy => obj_Copy
  !! This routine copies object
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Assignment operator
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate Data
  FINAL :: obj_Final

  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Add => obj_Add
  !! Add mesh IDs, elements, nodes or boxes to the selection
  PROCEDURE, PUBLIC, PASS(obj) :: Set => obj_Set
  !! Finish adding data, sorts and removes duplicates
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterialToMesh1 => obj_SetMaterialToMesh1
  !! Set material to the elements of a dimension
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterialToMesh2 => obj_SetMaterialToMesh2
  !! Set material to the elements of the top mesh
  GENERIC, PUBLIC :: SetMaterialToMesh => &
    SetMaterialToMesh1, SetMaterialToMesh2
  !! Set material to mesh

  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from the hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export to the HDF5File

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Displays the content

  ! @TomlMethods
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Initiate the object from a toml table
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Initiate the object from a toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Initiate the object from toml

  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshID => obj_GetMeshID
  !! Returns the mesh IDs of a dimension with allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshID_ => obj_GetMeshID_
  !! Returns the mesh IDs of a dimension without allocation

  ! @GetElemNumMethods
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalElemNum1 => obj_GetTotalElemNum1
  !! Size of the element list of a dimension
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalElemNum2 => obj_GetTotalElemNum2
  !! Number of selected elements of a dimension
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalElemNum3 => obj_GetTotalElemNum3
  !! Size of the element lists of all dimensions
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalElemNum4 => obj_GetTotalElemNum4
  !! Number of selected elements of all dimensions
  GENERIC, PUBLIC :: GetTotalElemNum => GetTotalElemNum1, GetTotalElemNum2, &
    GetTotalElemNum3, GetTotalElemNum4
  !! Returns the number of selected elements
  PROCEDURE, PRIVATE, PASS(obj) :: GetElemNum1 => obj_GetElemNum1
  !! Element list of a dimension
  PROCEDURE, PRIVATE, PASS(obj) :: GetElemNum2 => obj_GetElemNum2
  !! Selected elements of a dimension
  PROCEDURE, PRIVATE, PASS(obj) :: GetElemNum3 => obj_GetElemNum3
  !! Element lists of all dimensions
  PROCEDURE, PRIVATE, PASS(obj) :: GetElemNum4 => obj_GetElemNum4
  !! Selected elements of all dimensions
  GENERIC, PUBLIC :: GetElemNum => GetElemNum1, GetElemNum2, &
    GetElemNum3, GetElemNum4
  !! Returns the selected elements

  ! @GetNodeNumMethods
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalNodeNum1 => obj_GetTotalNodeNum1
  !! Size of nodeNum
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalNodeNum2 => obj_GetTotalNodeNum2
  !! Number of selected nodes of a dimension
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalNodeNum3 => obj_GetTotalNodeNum3
  !! Number of selected nodes of all dimensions
  GENERIC, PUBLIC :: GetTotalNodeNum => GetTotalNodeNum1, GetTotalNodeNum2, &
    GetTotalNodeNum3
  !! Returns the number of selected nodes
  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeNum1 => obj_GetNodeNum1
  !! nodeNum
  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeNum2 => obj_GetNodeNum2
  !! Selected nodes of a dimension
  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeNum3 => obj_GetNodeNum3
  !! Selected nodes of all dimensions
  GENERIC, PUBLIC :: GetNodeNum => GetNodeNum1, GetNodeNum2, &
    GetNodeNum3
  !! Returns the selected nodes

  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsMeshIDAllocated => &
    obj_IsMeshIDAllocated
  !! Returns true if the mesh ID list of a dimension is allocated
  PROCEDURE, PUBLIC, PASS(obj) :: isElemNumAllocated => &
    obj_isElemNumAllocated
  !! Returns true if the element list of a dimension is allocated
  PROCEDURE, PUBLIC, PASS(obj) :: isNodeNumAllocated => &
    obj_isNodeNumAllocated
  !! Returns true if nodeNum is allocated
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Returns the flags of the object
END TYPE MeshSelection_

!----------------------------------------------------------------------------
!                                                            MeshSelection_
!----------------------------------------------------------------------------

TYPE(MeshSelection_), PARAMETER :: TypeMeshSelection = MeshSelection_()

!----------------------------------------------------------------------------
!                                                     MeshSelectionPointer_
!----------------------------------------------------------------------------

TYPE :: MeshSelectionPointer_
  CLASS(MeshSelection_), POINTER :: ptr => NULL()
END TYPE MeshSelectionPointer_

!----------------------------------------------------------------------------
!                                             isInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Returns true if the object is initiated
!
! An object is initiated by Initiate, Import or ImportFromToml.
!
! Returns:
!   ans : LOGICAL(LGT) - true after Initiate, Import or ImportFromToml
!
! See also: [[obj_Initiate1]], [[obj_Deallocate]]

INTERFACE
  MODULE PURE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    LOGICAL(LGT) :: ans
    !! see Returns
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                    isSelectionByMeshID@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Returns true if the selection by mesh ID is active
!
! The flag is set by Initiate, and also by Add with meshID.
!
! Returns:
!   ans : LOGICAL(LGT) - true if the selection by mesh ID is active
!
! See also: [[obj_Initiate1]], [[obj_Add]], [[obj_GetParam]]

INTERFACE
  MODULE PURE FUNCTION obj_IsSelectionByMeshID(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    LOGICAL(LGT) :: ans
    !! see Returns
  END FUNCTION obj_IsSelectionByMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                    isSelectionByElemNum@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Returns true if the selection by element number is active
!
! The flag is set by Initiate, and also by Add with elemNum.
!
! Returns:
!   ans : LOGICAL(LGT) - true if the selection by element number is active
!
! See also: [[obj_Initiate1]], [[obj_Add]], [[obj_GetParam]]

INTERFACE
  MODULE PURE FUNCTION obj_IsSelectionByElemNum(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    LOGICAL(LGT) :: ans
    !! see Returns
  END FUNCTION obj_IsSelectionByElemNum
END INTERFACE

!----------------------------------------------------------------------------
!                                   isSelectionByNodeNum@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Returns true if the selection by node number is active
!
! The flag is set by Initiate, and also by Add with nodeNum.
!
! Returns:
!   ans : LOGICAL(LGT) - true if the selection by node number is active
!
! See also: [[obj_Initiate1]], [[obj_Add]], [[obj_GetParam]]

INTERFACE
  MODULE PURE FUNCTION obj_IsSelectionByNodeNum(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    LOGICAL(LGT) :: ans
    !! see Returns
  END FUNCTION obj_IsSelectionByNodeNum
END INTERFACE

!----------------------------------------------------------------------------
!                                       isSelectionByBox@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Returns true if the selection by bounding box is active
!
! The flag is set by Initiate, and also by Add with box.
!
! Returns:
!   ans : LOGICAL(LGT) - true if the selection by bounding box is active
!
! See also: [[obj_Initiate1]], [[obj_Add]], [[obj_GetParam]]

INTERFACE
  MODULE PURE FUNCTION obj_IsSelectionByBox(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    LOGICAL(LGT) :: ans
    !! see Returns
  END FUNCTION obj_IsSelectionByBox
END INTERFACE

!----------------------------------------------------------------------------
!                                             Reallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-09
! summary: Reallocate a vector of MeshSelection_
!
! If obj is not allocated then it is allocated. If its size is not tsize
! then it is deallocated and allocated again. If its size is tsize then it
! is not changed, and its instances keep their data.
!
! Examples
!
!```fortran
! TYPE(MeshSelection_), ALLOCATABLE :: vec(:)
! CALL MeshSelectionReallocate(vec, 2_I4B)
! CALL OK(SIZE(vec) .EQ. 2, "MeshSelectionReallocate")
!```
!
! Full example: src/modules/MeshSelection/examples/Add_test_1.F90
!
! See also: [[Deallocate_Vector]]

INTERFACE MeshSelectionReallocate
  MODULE SUBROUTINE Reallocate_Vector(obj, tsize)
    TYPE(MeshSelection_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of mesh selections
    INTEGER(I4B), INTENT(IN) :: tsize
    !! new size of obj
  END SUBROUTINE Reallocate_Vector
END INTERFACE MeshSelectionReallocate

!----------------------------------------------------------------------------
!                                             Reallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-09
! summary: Reallocate a vector of MeshSelectionPointer_
!
! See also: [[Reallocate_Vector]], [[Deallocate_Ptr_Vector]]

INTERFACE MeshSelectionReallocate
  MODULE SUBROUTINE Reallocate_Ptr_Vector(obj, tsize)
    TYPE(MeshSelectionPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of pointers
    INTEGER(I4B), INTENT(IN) :: tsize
    !! new size of obj
  END SUBROUTINE Reallocate_Ptr_Vector
END INTERFACE MeshSelectionReallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-09
! summary: Deallocate a vector of MeshSelection_
!
! See also: [[Reallocate_Vector]]

INTERFACE MeshSelectionDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(MeshSelection_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of mesh selections
  END SUBROUTINE Deallocate_Vector
END INTERFACE MeshSelectionDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-09
! summary: Deallocate a vector of MeshSelectionPointer_
!
! See also: [[Reallocate_Ptr_Vector]]

INTERFACE MeshSelectionDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(MeshSelectionPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of pointers
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE MeshSelectionDeallocate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Initiate an instance of MeshSelection_ (Deprecated)
!
! Deallocates obj, marks it as initiated and sets the selection flags from
! the options. The data is added later by Add, and Set is called when
! adding is finished.
!
! Examples
!
!```fortran
! TYPE(MeshSelection_) :: obj
! CALL obj%Initiate(isSelectionByMeshID=math%yes)
! CALL obj%Add(dim=2, meshID=[1, 2])
! CALL obj%Set()
! CALL OK(obj%IsSelectionByMeshID(), "Initiate: option meshID")
!```
!
! Full example: src/modules/MeshSelection/examples/Initiate_test_1.F90
!
! See also: [[obj_Initiate2]], [[obj_Add]], [[obj_Set]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1( &
    obj, isSelectionByMeshID, isSelectionByElemNum, isSelectionByBox, &
    isSelectionByNodeNum)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByMeshID
    !! selection by mesh ID, default is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByElemNum
    !! selection by element number, default is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByBox
    !! selection by box, default is false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSelectionByNodeNum
    !! selection by node number, default is false
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Copy@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-01
! summary: Copy an instance of MeshSelection_ by assignment
!
! Copies the flags and the lists of mesh IDs, element numbers and node
! numbers of obj2 to obj. The copy does not share data with obj2. The lists
! of obj which are not allocated in obj2 are removed.
!
! A box list is copied if it is allocated in obj2. Otherwise the box list
! of obj is removed.
!
! Changes:
!   - 2026-11-06: Box lists which are not allocated in obj2 are removed
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE GlobalData, ONLY: I4B
! USE MeshSelection_Class, ONLY: MeshSelection_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(MeshSelection_) :: obj, obj2
!
! CALL e%SetQuietMode(math%yes)
!
! CALL obj%Initiate()
! CALL obj%Add(dim=2, meshID=[2, 1])
! CALL obj%Add(dim=1, elemNum=[7, 8])
! CALL obj%Add(nodeNum=[4, 3])
! CALL obj%Set()
!
! ! Test 1: assignment copies the flags and the data
! BLOCK
!   INTEGER(I4B) :: ans(4), tsize
!
!   obj2 = obj
!   CALL OK(obj2%IsInitiated(), "Copy: IsInitiated")
!   CALL OK(obj2%IsSelectionByMeshID(), "Copy: meshID flag")
!   CALL OK(obj2%IsSelectionByElemNum(), "Copy: elemNum flag")
!   CALL OK(obj2%IsSelectionByNodeNum(), "Copy: nodeNum flag")
!   CALL OK(.NOT. obj2%IsSelectionByBox(), "Copy: box flag")
!   CALL OK(ALL(obj2%GetMeshID(dim=2) .EQ. [1, 2]), "Copy: meshID")
!
!   CALL obj2%GetElemNum(dim=1, ans=ans, tsize=tsize)
!   CALL OK(tsize .EQ. 2, "Copy: size of elemNum")
!   CALL OK(ALL(ans(1:tsize) .EQ. [7, 8]), "Copy: elemNum")
!
!   CALL obj2%GetNodeNum(ans=ans, tsize=tsize)
!   CALL OK(tsize .EQ. 2, "Copy: size of nodeNum")
!   CALL OK(ALL(ans(1:tsize) .EQ. [3, 4]), "Copy: nodeNum")
! END BLOCK
!
! ! Test 2: the copy does not share data with the source
! BLOCK
!   CALL obj%Add(dim=2, meshID=[9])
!   CALL OK(SIZE(obj%GetMeshID(dim=2)) .EQ. 3, "Copy: source grows")
!   CALL OK(SIZE(obj2%GetMeshID(dim=2)) .EQ. 2, "Copy: copy is unchanged")
! END BLOCK
!
! ! Test 3: the data of the destination is replaced by the source
! BLOCK
!   TYPE(MeshSelection_) :: other
!
!   CALL other%Initiate()
!   CALL other%Add(dim=3, meshID=[5])
!   obj2 = other
!   CALL OK(.NOT. obj2%IsSelectionByElemNum(), "Copy: replaces elemNum flag")
!   CALL OK(ALL(obj2%GetMeshID(dim=3) .EQ. [5]), "Copy: replaces meshID")
!   CALL OK(SIZE(obj2%GetMeshID(dim=2)) .EQ. 0, "Copy: drops old meshID")
!   CALL OK(.NOT. obj2%IsElemNumAllocated(dim=1), "Copy: drops old elemNum")
!   CALL OK(.NOT. obj2%IsNodeNumAllocated(), "Copy: drops old nodeNum")
!   CALL other%DEALLOCATE()
! END BLOCK
!
! CALL obj%DEALLOCATE()
! CALL obj2%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
    CLASS(MeshSelection_), INTENT(IN) :: obj2
    !! source of the copy
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Deallocate the data of MeshSelection_
!
! Removes every list and resets the flags. After this call IsInitiated is
! false.
!
! Examples
!
!```fortran
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "Deallocate: IsInitiated")
!```
!
! Full example: src/modules/MeshSelection/examples/Initiate_test_1.F90
!
! See also: [[obj_Initiate1]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Finalizer of MeshSelection_
!
! Calls [[obj_Deallocate]].

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Add@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Add mesh IDs, elements, nodes or boxes to the selection
!
! Each given list is appended to the list of dimension dim, so several
! lists can be given in one call. Each list sets its selection flag, even
! if Initiate did not set it.
!
! - meshID goes to pointMeshID, curveMeshID, surfaceMeshID or volumeMeshID.
! - elemNum goes to pointElemNum, curveElemNum, surfaceElemNum or
!   volumeElemNum.
! - nodeNum goes to pointNodeNum, curveNodeNum, surfaceNodeNum or
!   volumeNodeNum. Without dim it goes to nodeNum.
! - box goes to pointBox, curveBox, surfaceBox or volumeBox.
!
! Without dim the lists meshID, elemNum and box are ignored. Duplicates are
! kept until Set is called.
!
! Examples
!
!```fortran
! CALL obj%Initiate()
! CALL obj%Add(dim=0, meshID=[5, 3])
! CALL obj%Add(dim=0, meshID=[3, 1])
! CALL OK(ALL(obj%GetMeshID(dim=0) .EQ. [5, 3, 3, 1]), "Add: appends")
!```
!
! Full example: src/modules/MeshSelection/examples/Add_test_1.F90
!
! See also: [[obj_Set]], [[obj_GetMeshID]]

INTERFACE
  MODULE SUBROUTINE obj_Add(obj, dom, dim, meshID, box, elemNum, &
                            nodeNum)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
    !! not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of the list, 0 to 3; needed by meshID, elemNum and box
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshID(:)
    !! mesh IDs to append
    TYPE(BoundingBox_), OPTIONAL, INTENT(IN) :: box(:)
    !! bounding boxes to append
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemNum(:)
    !! global element numbers to append
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nodeNum(:)
    !! global node numbers to append
  END SUBROUTINE obj_Add
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Finish adding data to the selection
!
! Call this routine when no more data will be added.
!
! - The lists of mesh IDs and element numbers are sorted, and their
!   duplicates are removed.
! - The node lists of each dimension are appended to nodeNum, and nodeNum
!   is sorted and its duplicates are removed. The lists of each dimension
!   are kept.
! - Boxes are not changed.
!
! Calling it again gives the same selection.
!
! Examples
!
!```fortran
! CALL obj%Add(dim=0, meshID=[5, 3, 3, 1])
! CALL obj%Set()
! CALL OK(ALL(obj%GetMeshID(dim=0) .EQ. [1, 3, 5]), "Set: unique, sorted")
!```
!
! Full example: src/modules/MeshSelection/examples/Add_test_1.F90
!
! See also: [[obj_Add]], [[obj_Set2]]

INTERFACE
  MODULE SUBROUTINE obj_Set(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
  END SUBROUTINE obj_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Call Set for each instance of a vector
!
! This routine is available as MeshSelectionSet.
!
! Examples
!
!```fortran
! TYPE(MeshSelection_), ALLOCATABLE :: vec(:)
! CALL MeshSelectionReallocate(vec, 2_I4B)
! CALL vec(1)%Initiate()
! CALL vec(2)%Initiate()
! CALL vec(1)%Add(dim=1, meshID=[2, 2, 1])
! CALL vec(2)%Add(dim=1, meshID=[4, 4])
! CALL MeshSelectionSet(vec)
!```
!
! Full example: src/modules/MeshSelection/examples/Add_test_1.F90
!
! See also: [[obj_Set]]

INTERFACE MeshSelectionSet
  MODULE SUBROUTINE obj_Set2(obj)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj(:)
    !! vector of mesh selections
  END SUBROUTINE obj_Set2
END INTERFACE MeshSelectionSet

!----------------------------------------------------------------------------
!                                               SetMaterialToMesh@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-29
! summary: Set the material of the selected elements
!
! Sets the material of medium for the elements selected in the lists of
! dimension dim. The mesh of dimension dim is taken from dom, and its
! number of media must be set before, see SetTotalMedium of the domain.
!
! - For each mesh ID of dim it calls
!   mesh%SetMaterial(entityNum, material, medium).
! - For each element number of dim it calls
!   mesh%SetMaterial(medium, material, globalElement, islocal).
!
! The selections by node number and by box are not used.
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshSelection_Class, ONLY: MeshSelection_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = &
!   "../../FEMesh/examples/meshdata/very_small_quad4_mesh_two_region.h5"
! TYPE(MeshSelection_) :: obj
! TYPE(FEDomain_) :: dom
! TYPE(HDF5File_) :: meshfile
! CLASS(AbstractMesh_), POINTER :: mesh
!
! CALL e%SetQuietMode(math%yes)
! CALL meshfile%Initiate(filename, mode="READ")
! CALL meshfile%OPEN()
! CALL dom%Initiate(meshfile, '')
! CALL dom%SetTotalMedium(dim=2_I4B, n=2_I4B)
! CALL dom%SetTotalMedium(dim=1_I4B, n=1_I4B)
!
! ! Test 1: selection by mesh id and by element number, explicit dim
! BLOCK
!   mesh => dom%GetMeshPointer(dim=2)
!
!   CALL obj%Initiate()
!   CALL obj%Add(dim=2, meshID=[2])
!   CALL obj%Set()
!   CALL obj%SetMaterialToMesh(dom=dom, dim=2_I4B, medium=1_I4B, &
!                              material=5_I4B)
!   CALL OK(mesh%GetMaterial(medium=1, globalElement=20) .EQ. 5, &
!           "meshID: first element of entity")
!   CALL OK(mesh%GetMaterial(medium=1, globalElement=21) .EQ. 5, &
!           "meshID: second element of entity")
!
!   CALL obj%Initiate()
!   CALL obj%Add(dim=2, elemNum=[18])
!   CALL obj%Set()
!   CALL obj%SetMaterialToMesh(dom=dom, dim=2_I4B, medium=1_I4B, &
!                              material=7_I4B)
!   CALL OK(mesh%GetMaterial(medium=1, globalElement=18) .EQ. 7, &
!           "elemNum: selected element")
!   CALL OK(mesh%GetMaterial(medium=1, globalElement=19) .NE. 7, &
!           "elemNum: other element is unchanged")
!   CALL OK(mesh%GetMaterial(medium=1, globalElement=20) .EQ. 5, &
!           "elemNum: other entity is unchanged")
! END BLOCK
!
! ! Test 2: dim defaults to nsd, each medium has its own material
! BLOCK
!   CALL obj%Initiate()
!   CALL obj%Add(dim=2, meshID=[1])
!   CALL obj%Set()
!   CALL obj%SetMaterialToMesh(dom=dom, medium=2_I4B, material=3_I4B)
!   CALL OK(mesh%GetMaterial(medium=2, globalElement=19) .EQ. 3, &
!           "nsd: medium=2 is set")
!   CALL OK(mesh%GetMaterial(medium=1, globalElement=18) .EQ. 7, &
!           "nsd: medium=1 is unchanged")
! END BLOCK
!
! ! Test 3: lower dimensional mesh
! BLOCK
!   CLASS(AbstractMesh_), POINTER :: curve
!
!   curve => dom%GetMeshPointer(dim=1)
!   CALL obj%Initiate()
!   CALL obj%Add(dim=1, meshID=[1])
!   CALL obj%Set()
!   CALL obj%SetMaterialToMesh(dom=dom, dim=1_I4B, medium=1_I4B, &
!                              material=9_I4B)
!   CALL OK(curve%GetMaterial(medium=1, globalElement=7) .EQ. 9, &
!           "curve mesh: selected element")
!   curve => NULL()
! END BLOCK
!
! mesh => NULL()
! CALL obj%DEALLOCATE()
! CALL dom%DEALLOCATE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetMaterialToMesh2]]

INTERFACE
  MODULE SUBROUTINE obj_SetMaterialToMesh1(obj, dom, dim, medium, material)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
    CLASS(AbstractDomain_), INTENT(INOUT) :: dom
    !! domain, the materials of its mesh are changed
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the lists and of the mesh, 0 to 3
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium is like fluid, solid, etc. On a mesh we can have several
    !! overlapping mediums (for example in coupled problem)
    INTEGER(I4B), INTENT(IN) :: material
    !! material number of the medium. For example if medium is soil then
    !! material = 1, 2, 3 represents different types of soils
  END SUBROUTINE obj_SetMaterialToMesh1
END INTERFACE

INTERFACE MeshSelectionSetMaterialToMesh
  MODULE PROCEDURE obj_SetMaterialToMesh1
END INTERFACE MeshSelectionSetMaterialToMesh

!----------------------------------------------------------------------------
!                                               SetMaterialToMesh@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-29
! summary: Set the material of the selected elements of the top mesh
!
! Calls [[obj_SetMaterialToMesh1]] with dim=dom%GetNSD().
!
! See also: [[obj_SetMaterialToMesh1]]

INTERFACE
  MODULE SUBROUTINE obj_SetMaterialToMesh2(obj, dom, medium, material)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
    CLASS(AbstractDomain_), INTENT(INOUT) :: dom
    !! domain, the materials of its mesh are changed
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number
    INTEGER(I4B), INTENT(IN) :: material
    !! material number of the medium
  END SUBROUTINE obj_SetMaterialToMesh2
END INTERFACE

INTERFACE MeshSelectionSetMaterialToMesh
  MODULE PROCEDURE obj_SetMaterialToMesh2
END INTERFACE MeshSelectionSetMaterialToMesh

!----------------------------------------------------------------------------
!                                                           Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Initiate the object from an HDF5 file
!
! Reads the data written by Export from group. Datasets which are missing
! are skipped, and a list which is present sets its selection flag. Set is
! not needed afterwards, because Export writes the lists as they are.
!
! Boxes are not read, so the box flag can be true with no boxes.
!
! In a debug build obj must not be initiated, and the file must be open and
! readable.
!
! Examples
!
!```fortran
! CALL hdf5file%Initiate(filename=filename, mode="READ")
! CALL hdf5file%OPEN()
! CALL obj2%IMPORT(hdf5=hdf5file, group="selection")
! CALL hdf5file%DEALLOCATE()
! CALL OK(ALL(obj2%GetMeshID(dim=2) .EQ. [3, 8]), "Import: meshID dim=2")
!```
!
! Full example: src/modules/MeshSelection/examples/Export_test_1.F90
!
! See also: [[obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection, not initiated
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, it must be open
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
    !! not used
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-08
! summary: Initiate the object from a toml table
!
! Deallocates obj, reads the flags and the data from table, and calls Set.
! The table has this form.
!
!```toml
! isSelectionByMeshID = true
! isSelectionByElemNum = false
! isSelectionByNodeNum = false
! isSelectionByBox = false
!
! [meshID]
! point = [1, 2, 3]
! line = [2, 5]
! surface = [1, 3]
! volume = "./meshID_volume.txt"
!
! [elemNum]
! surface = [1, 2, 3]
!
! [nodeNum]
! point = [1, 2, 3]
!
! [box]
! surface = [
!   { xmin = 0.0, xmax = 1.0, ymin = 0.0, ymax = 1.0,
!     zmin = 0.0, zmax = 1.0 },
! ]
!```
!
! Each of the tables meshID, elemNum, nodeNum and box is optional, as is
! each of its keys point, line, surface and volume. A list can also be the
! name of a text file. A table which is present sets its flag to true, even
! if the flag is false in the file. In a debug build a true flag without
! its table is an error.
!
! Examples
!
!```fortran
! CALL obj%ImportFromToml(table=node)
! CALL OK(ALL(obj%GetMeshID(dim=1) .EQ. [2, 5, 6, 7]), &
!         "ImportFromToml(table): replaces old data")
!```
!
! Full example: src/modules/MeshSelection/examples/ImportFromToml_test_1.F90
!
! See also: [[obj_ImportFromToml2]], [[obj_ImportParamFromToml]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table of the selection
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
    !! not used
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-08
! summary: Initiate the object from a toml file
!
! Reads the table named tomlName from afile or filename, and calls
! [[obj_ImportFromToml1]]. In a debug build a missing table is an error.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE GlobalData, ONLY: I4B
! USE MeshSelection_Class, ONLY: MeshSelection_
! USE Test_Method, ONLY: OK
! USE tomlf, ONLY: toml_get => get_value
! USE tomlf, ONLY: toml_table
! USE TomlUtility, ONLY: GetValue
! IMPLICIT NONE
! CHARACTER(*), PARAMETER :: filename = "./toml/MeshSelection.toml"
! TYPE(MeshSelection_) :: obj
! INTEGER(I4B) :: ans(16), tsize
! CALL e%SetQuietMode(math%yes)
! ! Test 1: ImportFromToml reads every table of [region]
! BLOCK
!   CALL obj%ImportFromToml(tomlName="region", filename=filename)
!   CALL OK(obj%IsInitiated(), "ImportFromToml: IsInitiated")
!   CALL OK(obj%IsSelectionByMeshID(), "ImportFromToml: meshID flag")
!   CALL OK(obj%IsSelectionByElemNum(), &
!           "ImportFromToml: elemNum flag, table sets flag")
!   CALL OK(obj%IsSelectionByNodeNum(), "ImportFromToml: nodeNum flag")
!   CALL OK(obj%IsSelectionByBox(), "ImportFromToml: box flag")
!   CALL OK(ALL(obj%GetMeshID(dim=0) .EQ. [1, 2, 3, 4, 5, 6]), "meshID point")
!   CALL OK(ALL(obj%GetMeshID(dim=1) .EQ. [2, 5, 6, 7]), "meshID line")
!   CALL OK(ALL(obj%GetMeshID(dim=2) .EQ. [1, 3, 5, 6, 8]), "meshID surface")
!   CALL OK(ALL(obj%GetMeshID(dim=3) .EQ. [1, 2, 3]), "meshID from txt file")
!   CALL obj%GetElemNum(dim=2, ans=ans, tsize=tsize)
!   CALL OK(tsize .EQ. 3, "elemNum: size")
!   CALL OK(ALL(ans(1:tsize) .EQ. [1, 2, 3]), "elemNum: values")
!   ! Set has collected the nodes of every dimension
!   CALL obj%GetNodeNum(ans=ans, tsize=tsize)
!   CALL OK(tsize .EQ. 11, "nodeNum: size")
!   CALL OK(ALL(ans(1:tsize) .EQ. [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), &
!           "nodeNum: values")
!   CALL obj%DEALLOCATE()
! END BLOCK
! END PROGRAM main
!```
!
! See also: [[obj_ImportFromToml1]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, &
                                        filename, printToml, dom)
    CLASS(MeshSelection_), INTENT(INOUT) :: obj
    !! mesh selection
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the table in the toml file
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! toml file, it must be open; give afile or filename
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! name of the toml file; give afile or filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! not used
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
    !! not used
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Export the object to an HDF5 file
!
! Writes these datasets in group. A list is written only if it is
! allocated, and nothing is written if obj is not initiated.
!
! - isSelectionByMeshID, isSelectionByElemNum, isSelectionByNodeNum and
!   isSelectionByBox
! - PointMeshID, CurveMeshID, SurfaceMeshID and VolumeMeshID
! - PointElemNum, CurveElemNum, SurfaceElemNum and VolumeElemNum
! - NodeNum
!
! Boxes are not written. The node lists of each dimension are not written,
! so call Set first to collect them in NodeNum.
!
! In a debug build the file must be open and writable.
!
! Examples
!
!```fortran
! CALL obj%Set()
! CALL hdf5file%Initiate(filename=filename, mode="NEW")
! CALL hdf5file%OPEN()
! CALL obj%Export(hdf5=hdf5file, group="selection")
! CALL OK(hdf5file%PathExists("selection/NodeNum"), "Export: nodeNum")
! CALL hdf5file%DEALLOCATE()
!```
!
! Full example: src/modules/MeshSelection/examples/Export_test_1.F90
!
! See also: [[obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, it must be open with write access
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-28
! summary: Display the content of the object
!
! Writes msg, the flags, and the lists which are allocated. If obj is not
! initiated then only msg and the state are written.
!
! This routine is available as MeshSelectionDisplay.
!
! Changes:
!   - 2026-11-06: All lines are written to unitNo
!
! Examples
!
!```fortran
! OPEN (NEWUNIT=unitNo, STATUS="SCRATCH", ACTION="READWRITE")
! CALL MeshSelectionDisplay(obj, "selection", unitNo=unitNo)
! CLOSE (unitNo)
!```
!
! Full example: src/modules/MeshSelection/examples/Add_test_1.F90
!
! See also: [[obj_Display_Vector]], [[obj_Display_Ptr_Vector]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    CHARACTER(*), INTENT(IN) :: msg
    !! message to display first
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! unit to write to, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE MeshSelectionDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE MeshSelectionDisplay

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-27
! summary: Display the content of a vector of MeshSelection_
!
! See also: [[obj_Display]]

INTERFACE MeshSelectionDisplay
  MODULE SUBROUTINE obj_Display_Vector(obj, msg, unitNo)
    TYPE(MeshSelection_), INTENT(INOUT) :: obj(:)
    !! vector of mesh selections
    CHARACTER(*), INTENT(IN) :: msg
    !! message to display first
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! unit to write to, default is stdout
  END SUBROUTINE obj_Display_Vector
END INTERFACE MeshSelectionDisplay

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-27
! summary: Display the content of a vector of MeshSelectionPointer_
!
! See also: [[obj_Display]]

INTERFACE MeshSelectionDisplay
  MODULE SUBROUTINE obj_Display_Ptr_Vector(obj, msg, unitNo)
    TYPE(MeshSelectionPointer_), INTENT(INOUT) :: obj(:)
    !! vector of pointers
    CHARACTER(*), INTENT(IN) :: msg
    !! message to display first
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! unit to write to, default is stdout
  END SUBROUTINE obj_Display_Ptr_Vector
END INTERFACE MeshSelectionDisplay

!----------------------------------------------------------------------------
!                                                      GetMeshID@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Returns the mesh IDs of a dimension
!
! Returns a copy of the mesh ID list of dim.
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE - mesh IDs of dim, size zero if there
!     is no list of dim or dim is not in 0 to 3
!
! Examples
!
!```fortran
! CALL obj%Initiate()
! CALL obj%Add(dim=2, meshID=[3])
! CALL OK(ALL(obj%GetMeshID(dim=2) .EQ. [3]), "Add: dim=2")
! CALL OK(SIZE(obj%GetMeshID(dim=1)) .EQ. 0, "GetMeshID: empty list")
!```
!
! Full example: src/modules/MeshSelection/examples/Add_test_1.F90
!
! See also: [[obj_IsMeshIDAllocated]], [[obj_Add]]

INTERFACE
  MODULE FUNCTION obj_GetMeshID(obj, dim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the list
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! mesh IDs of dim
  END FUNCTION obj_GetMeshID
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMeshID_@GetMethods
!----------------------------------------------------------------------------
!> author: Vikas Sharma, Ph. D.
! date: 2026-11-07
! summary: GetMeshID of given dimension without allocation.
!
! This is method is similar to the [[obj_GetMeshID]] function.
! But this method does not allocates data.
!
INTERFACE
  MODULE SUBROUTINE obj_GetMeshID_(obj, dim, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the list
    INTEGER(I4B), INTENT(INOUT)  :: ans(:)
    !! mesh IDs of given dimension
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! amount of data written in ans
  END SUBROUTINE obj_GetMeshID_
END INTERFACE

!----------------------------------------------------------------------------
!                                              IsMeshIDAllocated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Returns true if the mesh ID list of dim is allocated
!
! The list is allocated by Add, Import or ImportFromToml.
!
! Returns:
!   ans : LOGICAL(LGT) - true if the list of dim is allocated, false if dim
!     is not in 0 to 3
!
! See also: [[obj_GetMeshID]]

INTERFACE
  MODULE PURE FUNCTION obj_IsMeshIDAllocated(obj, dim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the list
    LOGICAL(LGT) :: ans
    !! see Returns
  END FUNCTION obj_IsMeshIDAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                            isElemNumAllocated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Returns true if the element list of dim is allocated
!
! The list is allocated by Add, Import or ImportFromToml.
!
! Returns:
!   ans : LOGICAL(LGT) - true if the list of dim is allocated, false if dim
!     is not in 0 to 3
!
! See also: [[obj_GetElemNum1]]

INTERFACE
  MODULE PURE FUNCTION obj_IsElemNumAllocated(obj, dim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the list
    LOGICAL(LGT) :: ans
    !! see Returns
  END FUNCTION obj_IsElemNumAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                             isNodeNumAllocated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Returns true if nodeNum is allocated
!
! Only nodeNum is checked. The node lists of each dimension are collected
! in nodeNum by Set, so this function can be false before Set.
!
! Returns:
!   ans : LOGICAL(LGT) - true if nodeNum is allocated
!
! See also: [[obj_Set]], [[obj_GetNodeNum1]]

INTERFACE
  MODULE PURE FUNCTION obj_IsNodeNumAllocated(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    LOGICAL(LGT) :: ans
    !! see Returns
  END FUNCTION obj_IsNodeNumAllocated
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Returns the flags of the object
!
! Each present argument gets the value of the matching query function.
!
! Examples
!
!```fortran
! CALL obj%GetParam(isInitiated=isInit, isSelectionByMeshID=byMesh)
!```
!
! Full example: src/modules/MeshSelection/examples/Initiate_test_1.F90
!
! See also: [[obj_IsInitiated]], [[obj_IsSelectionByMeshID]]

INTERFACE
  MODULE PURE SUBROUTINE obj_GetParam( &
    obj, isInitiated, isSelectionByBox, isSelectionByMeshID, &
    isSelectionByElemNum, isSelectionByNodeNum)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated
    !! true if the object is initiated
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByBox
    !! true if the selection by box is active
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByMeshID
    !! true if the selection by mesh ID is active
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByElemNum
    !! true if the selection by element number is active
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isSelectionByNodeNum
    !! true if the selection by node number is active
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Returns the size of the element list of a dimension
!
! Mesh IDs are not counted, see [[obj_GetTotalElemNum2]].
!
! Returns:
!   ans : INTEGER(I4B) - size of the element list of dim, zero if the
!     selection by element number is not active
!
! See also: [[obj_GetElemNum1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalElemNum1(obj, dim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the list, 0 to 3
    INTEGER(I4B) :: ans
    !! size of the element list of dim
  END FUNCTION obj_GetTotalElemNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Returns the number of selected elements of a dimension
!
! Adds the elements of the mesh IDs of dim to the size of the element list
! of dim. An element which is in both selections is counted twice. Nodes
! and boxes are not used.
!
! If dom has no mesh of dimension dim then the mesh IDs of dim are skipped.
!
! Changes:
!   - 2026-11-06: A dimension with no mesh in dom is skipped
!
! Returns:
!   ans : INTEGER(I4B) - size needed by [[obj_GetElemNum2]]
!
! Examples
!
!```fortran
! CALL obj%Add(dim=2, meshID=[2])
! CALL obj%Set()
! CALL OK(obj%GetTotalElemNum(dim=2, dom=dom) .EQ. 2, "meshID: total dim=2")
!```
!
! Full example: src/modules/MeshSelection/examples/GetElemNum_test_1.F90
!
! See also: [[obj_GetElemNum2]]

INTERFACE
  MODULE FUNCTION obj_GetTotalElemNum2(obj, dim, dom) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the list, 0 to 3
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! domain which has the meshes
    INTEGER(I4B) :: ans
    !! number of selected elements of dim
  END FUNCTION obj_GetTotalElemNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Returns the size of the element lists of all dimensions
!
! Returns the sum of [[obj_GetTotalElemNum1]] for dim from 0 to 3.
!
! Returns:
!   ans : INTEGER(I4B) - size needed by [[obj_GetElemNum3]]
!
! See also: [[obj_GetElemNum3]]

INTERFACE
  MODULE FUNCTION obj_GetTotalElemNum3(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B) :: ans
    !! size of the element lists of all dimensions
  END FUNCTION obj_GetTotalElemNum3
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-29
! summary: Returns the number of selected elements of all dimensions
!
! Returns the sum of [[obj_GetTotalElemNum2]] for dim from 0 to 3. The
! mesh IDs of a dimension with no mesh in dom are skipped.
!
! Changes:
!   - 2026-11-06: A dimension with no mesh in dom is skipped
!
! Returns:
!   ans : INTEGER(I4B) - size needed by [[obj_GetElemNum4]]
!
! See also: [[obj_GetElemNum4]]

INTERFACE
  MODULE FUNCTION obj_GetTotalElemNum4(obj, dom) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! domain which has the meshes
    INTEGER(I4B) :: ans
    !! number of selected elements of all dimensions
  END FUNCTION obj_GetTotalElemNum4
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Get the element list of a dimension
!
! On return tsize is zero if the selection by element number is not active.
! Mesh IDs are not used, see [[obj_GetElemNum2]].
!
! Examples
!
!```fortran
! CALL obj%Add(dim=2, elemNum=[20, 18])
! CALL obj%Set()
! CALL obj%GetElemNum(dim=2, ans=ans, tsize=tsize)
! CALL OK(ALL(ans(1:tsize) .EQ. [18, 20]), "elemNum: values dim=2")
!```
!
! Full example: src/modules/MeshSelection/examples/GetElemNum_test_1.F90
!
! See also: [[obj_GetTotalElemNum1]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum1(obj, dim, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension: 0 point, 1 curve, 2 surface, 3 volume
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! element numbers, size at least obj%GetTotalElemNum(dim)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of element number
  END SUBROUTINE obj_GetElemNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! summary: Get the selected elements of a dimension
!
! Returns the element list of dim, followed by the elements of each mesh
! ID of dim. Element numbers are global. An element which is in both
! selections comes twice. Selections by node number and box are not used.
!
! If dom has no mesh of dimension dim then the mesh IDs of dim are skipped.
!
! Changes:
!   - 2026-11-06: A dimension with no mesh in dom is skipped
!
! Examples
!
!```fortran
! CALL obj%Add(dim=2, elemNum=[18])
! CALL obj%Add(dim=2, meshID=[2])
! CALL obj%Set()
! CALL obj%GetElemNum(dim=2, dom=dom, ans=ans, tsize=tsize)
! CALL OK(ALL(ans(1:tsize) .EQ. [18, 20, 21]), "both: values")
!```
!
! Full example: src/modules/MeshSelection/examples/GetElemNum_test_1.F90
!
! See also: [[obj_GetTotalElemNum2]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum2(obj, dim, dom, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension: 0 point, 1 curve, 2 surface, 3 volume
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! abstract domain
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global element numbers, size at least obj%GetTotalElemNum(dim, dom)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of element number
  END SUBROUTINE obj_GetElemNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Get the element lists of all dimensions
!
! Calls [[obj_GetElemNum1]] for dim from 0 to 3, and joins the results.
!
! See also: [[obj_GetTotalElemNum3]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum3(obj, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! element numbers, size at least obj%GetTotalElemNum()
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to ans
  END SUBROUTINE obj_GetElemNum3
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemNum@GetElemNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! summary: Get the selected elements of all dimensions
!
! Calls [[obj_GetElemNum2]] for dim from 0 to 3, and joins the results. The
! mesh IDs of a dimension with no mesh in dom are skipped.
!
! Changes:
!   - 2026-11-06: A dimension with no mesh in dom is skipped
!
! See also: [[obj_GetTotalElemNum4]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum4(obj, dom, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! domain which has the meshes
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global element numbers, size at least obj%GetTotalElemNum(dom)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to ans
  END SUBROUTINE obj_GetElemNum4
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalNodeNum@GetNodeNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Returns the size of nodeNum
!
! The node lists of each dimension are in nodeNum only after Set.
!
! Returns:
!   ans : INTEGER(I4B) - size needed by [[obj_GetNodeNum1]]
!
! See also: [[obj_GetNodeNum1]], [[obj_Set]]

INTERFACE
  MODULE FUNCTION obj_GetTotalNodenum1(obj) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B) :: ans
    !! size of nodeNum
  END FUNCTION obj_GetTotalNodenum1
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalNodeNum@GetNodeNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Returns the number of selected nodes of a dimension
!
! Returns the sum of these numbers.
!
! - The size of nodeNum, unless onlydim is true.
! - The number of nodes of each mesh ID of dim.
! - The number of unique nodes of the element list of dim.
!
! A node which is in more than one of these, or in two mesh IDs, is counted
! each time.
!
! Changes:
!   - 2026-11-06: onlydim is read by value, so .FALSE. keeps nodeNum
!
! Returns:
!   ans : INTEGER(I4B) - size needed by [[obj_GetNodeNum2]]
!
! Examples
!
!```fortran
! CALL obj%Add(nodeNum=[10, 9])
! CALL obj%Add(dim=2, meshID=[2])
! CALL obj%Set()
! CALL OK(obj%GetTotalNodeNum(dim=2, dom=dom) .EQ. 8, "dim: with nodeNum")
!```
!
! Full example: src/modules/MeshSelection/examples/GetNodeNum_test_1.F90
!
! See also: [[obj_GetNodeNum2]]

INTERFACE
  MODULE FUNCTION obj_GetTotalNodenum2(obj, dim, dom, onlydim) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh and of the lists
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! domain which has the meshes
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlydim
    !! if true then nodeNum is not counted, default is false
    INTEGER(I4B) :: ans
    !! number of selected nodes of dim
  END FUNCTION obj_GetTotalNodenum2
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalNodeNum@GetNodeNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Returns the number of selected nodes of all dimensions
!
! Returns the sum of [[obj_GetTotalNodeNum2]] for dim from 0 to the spatial
! dimension of dom. The size of nodeNum is added once. Lists of a dimension
! above the spatial dimension are skipped.
!
! Returns:
!   ans : INTEGER(I4B) - size needed by [[obj_GetNodeNum3]]
!
! See also: [[obj_GetNodeNum3]]

INTERFACE
  MODULE FUNCTION obj_GetTotalNodenum3(obj, dom) RESULT(ans)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! domain which has the meshes
    INTEGER(I4B) :: ans
    !! number of selected nodes of all dimensions
  END FUNCTION obj_GetTotalNodenum3
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetNodeNum@GetNodeNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Get nodeNum
!
! The node lists of each dimension are in nodeNum only after Set. On return
! tsize is zero if nodeNum is empty.
!
! Examples
!
!```fortran
! CALL obj%Add(nodeNum=[10, 9])
! CALL obj%Set()
! CALL obj%GetNodeNum(ans=ans, tsize=tsize)
! CALL OK(ALL(ans(1:tsize) .EQ. [9, 10]), "nodeNum: values")
!```
!
! Full example: src/modules/MeshSelection/examples/GetNodeNum_test_1.F90
!
! See also: [[obj_GetTotalNodeNum1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeNum1(obj, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node numbers, size at least obj%GetTotalNodeNum()
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to ans
  END SUBROUTINE obj_GetNodeNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetNodeNum@GetNodeNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Get the selected nodes of a dimension
!
! Returns nodeNum (unless onlydim is true), followed by the nodes of
! each mesh ID of dim and the unique nodes of the element list of dim. The
! result can have repeated nodes, see [[obj_GetTotalNodeNum2]]. The
! selection by box is not used.
!
! Changes:
!   - 2026-11-06: onlydim is read by value, so .FALSE. keeps nodeNum
!
! Examples
!
!```fortran
! CALL obj%Add(dim=2, meshID=[2])
! CALL obj%Set()
! CALL obj%GetNodeNum(dim=2, dom=dom, ans=ans, tsize=tsize)
! CALL OK(tsize .EQ. 6, "meshID: size")
!```
!
! Full example: src/modules/MeshSelection/examples/GetNodeNum_test_1.F90
!
! See also: [[obj_GetTotalNodeNum2]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeNum2(obj, dim, dom, ans, tsize, onlydim)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh and of the lists
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! domain which has the meshes
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node numbers, size at least obj%GetTotalNodeNum(dim, dom)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to ans
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlydim
    !! if true then nodeNum is not used, default is false
  END SUBROUTINE obj_GetNodeNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetNodeNum@GetNodeNumMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-31
! summary: Get the selected nodes of all dimensions
!
! Calls [[obj_GetNodeNum2]] for dim from 0 to the spatial dimension of dom,
! and joins the results. nodeNum is used once. The result can have repeated
! nodes.
!
! See also: [[obj_GetTotalNodeNum3]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeNum3(obj, dom, ans, tsize)
    CLASS(MeshSelection_), INTENT(IN) :: obj
    !! mesh selection
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    !! domain which has the meshes
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node numbers, size at least obj%GetTotalNodeNum(dom)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written to ans
  END SUBROUTINE obj_GetNodeNum3
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MeshSelection_Class

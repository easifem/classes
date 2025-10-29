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

MODULE AbstractMeshField_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: FEVariable_
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class, ONLY: AbstractField_
USE FieldOpt_Class, ONLY: typefield => TypeFieldOpt
USE HDF5File_Class, ONLY: HDF5File_
USE VTKFile_Class, ONLY: VTKFile_
USE UserFunction_Class, ONLY: UserFunction_, UserFunctionPointer_
USE FEDOF_Class, ONLY: FEDOF_
USE AbstractMaterial_Class, ONLY: AbstractMaterial_, &
                                  AbstractMaterialPointer_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractMeshField_Class"

CHARACTER(*), PARAMETER :: AbstractMeshFieldEssential = "/name/fieldType"// &
                           "/engine/defineOn/varType/rank/s/totalShape"

PUBLIC :: AbstractMeshField_
PUBLIC :: SetAbstractMeshFieldParam
PUBLIC :: AbstractMeshFieldCheckEssentialParam
PUBLIC :: AbstractMeshFieldDeallocate
PUBLIC :: AbstractMeshFieldInitiate
PUBLIC :: AbstractMeshFieldGetShapeAndSize

!----------------------------------------------------------------------------
!                                                         AbstractMeshField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-11
! summary: Abstract node field

TYPE, ABSTRACT :: AbstractMeshField_
  PRIVATE

  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is true if the object is initiated

  INTEGER(I4B) :: fieldType = typefield%normal
  !! fieldType can be normal, constant, can vary in space and/ or both.

  TYPE(String) :: name
  !! name of the field

  TYPE(String) :: engine
  !! Engine of the field, for example
  !! NATIVE_SERIAL, ! NATIVE_OMP, ! NATIVE_MPI, ! PETSC, ! LIS_SERIAL,
  !! LIS_OMP, ! LIS_MPI

  INTEGER(I4B) :: tSize = 0
  !! total number of elements

  INTEGER(I4B) :: defineOn = 0
  !! Nodal: nodal values
  !! Quadrature: quadrature values

  INTEGER(I4B) :: rank = 0
  !! Scalar ! Vector ! Matrix

  INTEGER(I4B) :: varType = 0
  !! Space ! Time ! SpaceTime ! Constant

  INTEGER(I4B) :: totalShape = 0
  !! total shape of the data

  INTEGER(I4B), ALLOCATABLE :: ss(:)
  !! shape of the data

  INTEGER(I4B), ALLOCATABLE :: indxShape(:)
  !! Index for shape

  INTEGER(I4B), ALLOCATABLE :: indxVal(:)
  !! Index for value, The size of indxVal is equal to tElements+1
  !! indxVal(iel) gives the starting index of the element iel
  !! indxVal(iel+1)-indxVal(iel) gives the total number of values in element
  !! iel

  REAL(DFP), ALLOCATABLE :: val(:)
  !! values, val( :, iel ) corresponds to element number iel
  !! iel is local element number
  !! also, note that val( :, iel ) will be decoded
  !! based on the information stored in s(:)

  CLASS(AbstractMesh_), POINTER :: mesh => NULL()
  !! Mesh which contains the information of the finite element.

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! Check essential parameters
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate the field by reading param and a given mesh
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by copying other fields, and different options
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate from Abstract materials
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Initiate5 => obj_Initiate5
  !! Initiate from user function
  !! This routine should be implemened by the child class
  PROCEDURE, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate from user function
  !! This routine should be implemened by the child class
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3, &
    Initiate4, Initiate5
  !! Generic initiate
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Deallocate the field

  ! IO:
  ! @IOMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from hdf5 file
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data in hdf5 file
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: WriteData_vtk => &
    obj_WriteData_vtk
  !! Export data in vtkFile
  GENERIC, PUBLIC :: WriteData => WriteData_vtk
  !! Write data in VTK file

  ! GET:
  ! @GetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Shape => obj_Shape
  !! Return shape
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Get => obj_Get
  !! Getting the value
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix

  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Returns true if the object is initiated

  ! SET:
  ! @AddMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Add1 => obj_Add1
  !! Adding a value to an element
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Add2 => obj_Add2
  !! Add a value to all the elements
  GENERIC, PUBLIC :: Add => Add1, Add2

  ! SET:
  ! @SetMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Setting the value by using FEVariable_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Setting the value by using UserFunction_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Setting the value by using material
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set all values by using the FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set all values by using the FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set the values by using AbstractMaterialPointer_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set the values by using user function pointers

  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, Set7

  ! SET:
  ! @InsertMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert1 => obj_Insert1
  !! Insertting the value by using FEVariable_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert2 => obj_Insert2
  !! Insertting the value by using UserFunction_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert3 => obj_Insert3
  !! Insertting the value by using material
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert4 => obj_Insert4
  !! Insert all values by using the FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert5 => obj_Insert5
  !! Insert globalElement value by using user function
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert6 => obj_Insert6
  !!  Insert the values by using AbstractMaterialPointer
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert7 => obj_Insert7
  !! Insert the values by using user functions

  GENERIC, PUBLIC :: Insert => Insert1, Insert2, Insert3, Insert4, &
    Insert5, Insert6, Insert7
END TYPE AbstractMeshField_

!----------------------------------------------------------------------------
!                              SetAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine Check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE SetAbstractMeshFieldParam( &
    param, prefix, name, fieldType, engine, defineOn, varType, rank, s)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    !! prefix
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type
    CHARACTER(*), INTENT(IN) :: engine
    !! engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! define on Nodal or Quadrature
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type
    !! how the field varies inside the element
    !! space, time, spaceTime, constant
    INTEGER(I4B), INTENT(IN) :: rank
    !! rank of the field, scalar, vector, matrix
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of the field
  END SUBROUTINE SetAbstractMeshFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine Check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

INTERFACE AbstractMeshFieldCheckEssentialParam
  MODULE PROCEDURE obj_CheckEssentialParam
END INTERFACE AbstractMeshFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate the field by reading param and given domain

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE obj_Initiate1
END INTERFACE

INTERFACE AbstractMeshFieldInitiate
  MODULE PROCEDURE obj_Initiate1
END INTERFACE AbstractMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate by copying other fields, and different options

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate from abstractMaterials
!
!# Introduction
!
! We first search the name in material
! If the name is found in the material  then we get the pointer to
! user function corresponding to the material name.
! Then we call Initiate4 method

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, mesh, material, name, engine, nnt)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(AbstractMaterial_), INTENT(INOUT) :: material
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the material
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE obj_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate from UserFunction_

INTERFACE
  MODULE SUBROUTINE obj_Initiate4(obj, mesh, func, name, engine, nnt)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE obj_Initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-29
! summary:  Initiate AbstractMeshField_ from arguments
!
!# Introduction
!   This routine is used to initiate the AbstractMeshField_ from
!   arguments.
!   It is like Initiate1, but it does not use ParameterList_

INTERFACE
  MODULE SUBROUTINE obj_Initiate5( &
    obj, name, fieldType, engine, defineOn, varType, rank, s, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type
    CHARACTER(*), INTENT(IN) :: engine
    !! engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! define on Nodal or Quadrature
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type
    !! how the field varies inside the element
    !! space, time, spaceTime, constant
    INTEGER(I4B), INTENT(IN) :: rank
    !! rank of the field, scalar, vector, matrix
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of the field
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE obj_Initiate5
END INTERFACE

INTERFACE AbstractMeshFieldInitiate
  MODULE PROCEDURE obj_Initiate5
END INTERFACE AbstractMeshFieldInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Oct 2021
! summary: Deallocates data in [[AbstractMeshField_]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE AbstractMeshFieldDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE AbstractMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                                           Shape@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This function returns the shape of data

INTERFACE
  MODULE FUNCTION obj_Shape(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! Returned value depends upon the obj%vartype
    !! If constant then
    !!   - ans = [1] for constant
    !!   - ans = obj%s(1:1) for space and time
    !!   - ans = obj%s(1:2) for space-time
    !! If Vector then
    !!  - ans = obj%s(1:1) for constant
    !!  - ans = obj%s(1:2) for space and time
    !!  - ans = obj%s(1:3) for space-time
    !! If Matrix then
    !!  - ans = obj%s(1:2) for constant
    !!  - ans = obj%s(1:3) for space and time
    !!  - ans = obj%s(1:4) for space-time
  END FUNCTION obj_Shape
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Get the values from AbstractMeshField_

INTERFACE
  MODULE SUBROUTINE obj_Get(obj, globalElement, fevar, islocal)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then global element is local element
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Get
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-03
! summary:  Get the prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary:  Returns obj%isInit

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                  ScalarMeshFieldGetShapeAndSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-31
! summary:  Get shape and size of scalar and stscalar within the element

INTERFACE
  MODULE SUBROUTINE ScalarMeshFieldGetShapeAndSize(varType, s, tsize, &
                                                   nns, nnt)
    INTEGER(I4B), INTENT(IN) :: varType
    INTEGER(I4B), INTENT(INOUT) :: s(4)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nns
    !! number of nodes or quadrature points in an element
    !! nns should be present when varType is space or spaceTime
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes or quadrature points in a time element
    !! nnt should be present when varType is time or spaceTime
  END SUBROUTINE ScalarMeshFieldGetShapeAndSize
END INTERFACE

!----------------------------------------------------------------------------
!                                  ScalarMeshFieldGetShapeAndSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-31
! summary:  Get shape and size of scalar and stscalar within the element

INTERFACE
  MODULE SUBROUTINE VectorMeshFieldGetShapeAndSize( &
    varType, s, tsize, spaceCompo, nns, nnt)
    INTEGER(I4B), INTENT(IN) :: varType
    INTEGER(I4B), INTENT(INOUT) :: s(4)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space components
    !! spaceCompo should be present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nns
    !! number of nodes or quadrature points in an element
    !! nns should be present when varType is space or spaceTime
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes or quadrature points in a time element
    !! nnt should be present when varType is time or spaceTime
  END SUBROUTINE VectorMeshFieldGetShapeAndSize
END INTERFACE

!----------------------------------------------------------------------------
!                                  TensorMeshFieldGetShapeAndSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-31
! summary:  Get shape and size of tensor and sttensor within the element

INTERFACE
  MODULE SUBROUTINE TensorMeshFieldGetShapeAndSize( &
    varType, s, tsize, dim1, dim2, nns, nnt)
    INTEGER(I4B), INTENT(IN) :: varType
    INTEGER(I4B), INTENT(INOUT) :: s(4)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim1, dim2
    !! dim1 and dim2 of the tensor mesh field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nns
    !! number of nodes or quadrature points in an element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes or quadrature points in a time element
  END SUBROUTINE TensorMeshFieldGetShapeAndSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetShapeAndSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-31
! summary:  Get shape and size of data within the element

INTERFACE
  MODULE SUBROUTINE AbstractMeshFieldGetShapeAndSize( &
    rank, varType, s, tsize, spaceCompo, dim1, dim2, nns, nnt)
    INTEGER(I4B), INTENT(IN) :: rank
    INTEGER(I4B), INTENT(IN) :: varType
    INTEGER(I4B), INTENT(INOUT) :: s(4)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space components
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim1, dim2
    !! dim1 and dim2 of the tensor mesh field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nns, nnt
  END SUBROUTINE AbstractMeshFieldGetShapeAndSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           IMPORT@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Import from hdf5file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(AbstractMesh_), TARGET, OPTIONAL, INTENT(IN) :: mesh
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Export to hdf5file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Export to vtk file

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk(obj, nodeCoordField, filename)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(AbstractMeshField_), INTENT(INOUT) :: nodeCoordField
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE obj_WriteData_vtk
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Set values in AbstractMeshField_ from FEVariable
!
!# Introduction
! This routine sets the values in AbstractMeshField_ from FEVariable.

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalElement, islocal, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global element is local element
    TYPE(FEVariable_), INTENT(IN) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Set values in AbstractMeshField_
!
!# Introduction
! This routine sets the values in AbstractMeshField_ from UserFunction.
!
! - For each element of the mesh we get coordinates
! - Then we get value from userfunction by using the coordinates
! - Then we set the value in AbstractMeshField_

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Set values in AbstractMeshField_ by AbstractMaterial
!
!# Introduction
! This routine sets the values in AbstractMeshField_ from
! AbstractMaterial.
! Step 1: First we get the usefucntion from material by using the name
! Step 2: Then we call Set2 method to set the values in AbstractMeshField_

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, material, name, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(AbstractMaterial_), INTENT(INOUT) :: material
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Set values in AbstractMeshField_ using FeVariable
!
!# Introduction
!
! This routine sets the values in AbstractMeshField_ from FEVariable.

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: fevar
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Set values in AbstractMeshField_ by UserFunction
!
!# Introduction
! This routine sets the value of globalElement in AbstractMeshField_
! from user function. This function is like Set2, but in this case
! we set the value of a single element.

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, func, globalElement, islocal, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Set AbstractMeshField_ using AbstractMaterial Add
!          MeshSelection_
!
!# Introduction
! This routine sets the values in AbstractMeshField_ from
! AbstractMaterial. The following steps are performed:
!
! 1. The elements of mesh contains the medium and material information
! 2. We first get the material number which acts as a pointer to
!    material.
! 3. Then from the material we get the user function by using the name
! 4. Then we set the values in that element using this function

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, medium, material, name, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(AbstractMaterialPointer_), INTENT(INOUT) :: material(:)
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the material
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set6
END INTERFACE

INTERFACE AbstractMeshFieldSet
  MODULE PROCEDURE obj_Set6
END INTERFACE AbstractMeshFieldSet

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Set AbstractMeshField_ using AbstractMaterial Add
!          MeshSelection_
!
!# Introduction
! This routine sets the values in AbstractMeshField_ from
! AbstractMaterial. The following steps are performed:
!
! 1. The elements of mesh contains the medium and material information
! 2. We first get the material number which acts as a pointer to
!    material.
! 3. Then from the material we get the user function by using the name
! 4. Then we set the values in that element using this function

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, medium, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(UserFunctionPointer_), INTENT(INOUT) :: func(:)
    !! Abstract material
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set7
END INTERFACE

INTERFACE AbstractMeshFieldSet
  MODULE PROCEDURE obj_Set7
END INTERFACE AbstractMeshFieldSet

!----------------------------------------------------------------------------
!                                                       Insert@InsertMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-14
! summary: Insert values in AbstractMeshField_
!
!# Introduction
! This routine inserts the values in AbstractMeshField_ for globalElement
! from FEVariable.

INTERFACE
  MODULE SUBROUTINE obj_Insert1(obj, globalElement, islocal, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global element is local element
    TYPE(FEVariable_), INTENT(IN) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Insert1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Insert@InsertMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Insert values in AbstractMeshField_
!
!# Introduction
!
! This routine inserts the values in AbstractMeshField_ from UserFunction
! for globalElement.
! It performs following steps:
!
! 1. Get the FEVariable_ from UserFunction
! 2. Call obj_Insert1 method to insert the values in AbstractMeshField_

INTERFACE
  MODULE SUBROUTINE obj_Insert2(obj, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Insert@InsertMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Insert values in AbstractMeshField_ by AbstractMaterial
!
!# Introduction
! This routine inserts the values in AbstractMeshField_ from
! AbstractMaterial.
!
! Step 1: First we get the usefucntion from material by using the name
! Step 2: Then we call Insert2 method to insert the values

INTERFACE
  MODULE SUBROUTINE obj_Insert3(obj, material, name, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(AbstractMaterial_), INTENT(INOUT) :: material
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Insert@InsertMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Insert values in AbstractMeshField_

INTERFACE
  MODULE SUBROUTINE obj_Insert4(obj, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: fevar
  END SUBROUTINE obj_Insert4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-18
! summary: Insert values in AbstractMeshField from userFunction
!
!# Introduction
! This routine sets the value of globalElement in AbstractMeshField_
! from user function. This function is like Insert2, but in this case
! we insert the value of a single element.

INTERFACE
  MODULE SUBROUTINE obj_Insert5(obj, func, globalElement, islocal, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Insert@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Insert AbstractMeshField_ using AbstractMaterial Add
!          MeshSelection_
!
!# Introduction
!
! This routine Inserts the values in AbstractMeshField_ from
! AbstractMaterial. The following steps are performed:
!
! 1. The elements of mesh contains the medium and material information
! 2. We first get the material number which acts as a pointer to
!    material.
! 3. Then from the material we get the user function by using the name
! 4. Then we Insert the values in that element using this function

INTERFACE
  MODULE SUBROUTINE obj_Insert6(obj, medium, material, name, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(AbstractMaterialPointer_), INTENT(INOUT) :: material(:)
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the material
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert6
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Insert AbstractMeshField_ using AbstractMaterial Add
!          MeshSelection_
!
!# Introduction
! This routine Inserts the values in AbstractMeshField_ from
! AbstractMaterial. The following steps are performed:
!
! 1. The elements of mesh contains the medium and material information
! 2. We first get the material number which acts as a pointer to
!    material.
! 3. Then from the material we get the user function by using the name
! 4. Then we Insert the values in that element using this function

INTERFACE
  MODULE SUBROUTINE obj_Insert7(obj, medium, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(UserFunctionPointer_), INTENT(INOUT) :: func(:)
    !! Abstract material
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert7
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Add values to AbstractMeshField_
!
!# Introduction
!
! Add values to AbstractMeshField_ form FEVariable.
! This routine can be used while updating the abstractMeshField

INTERFACE
  MODULE SUBROUTINE obj_Add1(obj, globalElement, islocal, scale, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), INTENT(IN) :: islocal
    REAL(DFP), INTENT(IN) :: scale
    TYPE(FEVariable_), INTENT(IN) :: fevar
  END SUBROUTINE obj_Add1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Add values to AbstractMeshField_

INTERFACE
  MODULE SUBROUTINE obj_Add2(obj, scale, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: scale
    TYPE(FEVariable_), INTENT(IN) :: fevar
  END SUBROUTINE obj_Add2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractMeshField_Class

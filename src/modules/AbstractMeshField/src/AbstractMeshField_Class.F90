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

USE BaseType, ONLY: MAX_RANK_FEVARIABLE, &
                    FEVariable_

USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class, ONLY: AbstractField_, TypeField
USE HDF5File_Class, ONLY: HDF5File_
USE VTKFile_Class, ONLY: VTKFile_
USE AbstractMaterial_Class, ONLY: AbstractMaterial_
USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractMeshField_Class"

CHARACTER(*), PARAMETER :: AbstractMeshFieldEssential = "/name/fieldType"// &
                           "/engine/defineOn/varType/rank/s/totalShape"

PUBLIC :: AbstractMeshField_
PUBLIC :: AbstractMeshFieldPointer_
PUBLIC :: SetAbstractMeshFieldParam
PUBLIC :: AbstractMeshFieldCheckEssentialParam
PUBLIC :: AbstractMeshFieldDeallocate
PUBLIC :: AbstractMeshFieldInitiate

PUBLIC :: AbstractScalarMeshField_
PUBLIC :: AbstractScalarMeshFieldPointer_
PUBLIC :: AbstractVectorMeshField_
PUBLIC :: AbstractVectorMeshFieldPointer_
PUBLIC :: AbstractTensorMeshField_
PUBLIC :: AbstractTensorMeshFieldPointer_

!----------------------------------------------------------------------------
!                                                         AbstractMeshField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-11
! summary: Abstract node field

TYPE, ABSTRACT :: AbstractMeshField_
  PRIVATE

  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is true if the object is initiated
  INTEGER(I4B) :: fieldType = TypeField%normal
  !! fieldType can be normal, constant, can vary in space and/ or both.
  TYPE(String) :: name
  !! name of the field
  TYPE(String) :: engine
  !! Engine of the field, for example
  !! NATIVE_SERIAL,
  !! NATIVE_OMP,
  !! NATIVE_MPI,
  !! PETSC,
  !! LIS_SERIAL,
  !! LIS_OMP,
  !! LIS_MPI
  INTEGER(I4B) :: tSize = 0
  !! total number of elements
  INTEGER(I4B) :: defineOn = 0
  !! Nodal: nodal values
  !! Quadrature: quadrature values
  INTEGER(I4B) :: rank = 0
  !! Scalar
  !! Vector
  !! Matrix
  INTEGER(I4B) :: varType = 0
  !! Space
  !! Time
  !! SpaceTime
  !! Constant
  INTEGER(I4B) :: s(MAX_RANK_FEVARIABLE) = 1
  !! shape of the data
  REAL(DFP), ALLOCATABLE :: val(:, :)
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

  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  !! Check essential parameters

  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate the field by reading param and a given mesh

  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by copying other fields, and different options

  PROCEDURE, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate from Abstract materials

  PROCEDURE, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate from user function

  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3, &
    Initiate4
  !! Generic initiate

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the field

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from hdf5 file

  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data in hdf5 file

  PROCEDURE, PUBLIC, PASS(obj) :: ExportInVTK => obj_ExportInVTK
  !! Export data in vtkFile

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Returns size

  PROCEDURE, PUBLIC, PASS(obj) :: Shape => obj_Shape
  !! Return shape

  PROCEDURE, PUBLIC, PASS(obj) :: Get => obj_Get
  !! Getting the value

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix

  ! SET:
  ! @AddMethods

  PROCEDURE, PASS(obj) :: Add1 => obj_Add1
  !! Adding a value to an element

  PROCEDURE, PASS(obj) :: Add2 => obj_Add2
  !! Add a value to all the elements

  GENERIC, PUBLIC :: Add => Add1, Add2

  ! SET:
  ! @SetMethods

  PROCEDURE, PASS(obj) :: Set1 => obj_Set1
  !! Setting the value by using FEVariable_
  PROCEDURE, PASS(obj) :: Set2 => obj_Set2
  !! Setting the value by using UserFunction_
  PROCEDURE, PASS(obj) :: Set3 => obj_Set3
  !! Setting the value by using material

  PROCEDURE, PASS(obj) :: Set4 => obj_Set4
  !! Setting the value by using material

  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4

END TYPE AbstractMeshField_

!----------------------------------------------------------------------------
!                                                 AbstractScalarMeshField_
!----------------------------------------------------------------------------

TYPE, EXTENDS(AbstractMeshField_) :: AbstractScalarMeshField_
END TYPE AbstractScalarMeshField_

!----------------------------------------------------------------------------
!                                           AbstractScalarMeshFieldPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractScalarMeshFieldPointer_
  CLASS(AbstractScalarMeshField_), POINTER :: ptr => NULL()
END TYPE AbstractScalarMeshFieldPointer_

!----------------------------------------------------------------------------
!                                                 AbstractVectorMeshField_
!----------------------------------------------------------------------------

TYPE, EXTENDS(AbstractMeshField_) :: AbstractVectorMeshField_
END TYPE AbstractVectorMeshField_

!----------------------------------------------------------------------------
!                                           AbstractVectorMeshFieldPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractVectorMeshFieldPointer_
  CLASS(AbstractVectorMeshField_), POINTER :: ptr => NULL()
END TYPE AbstractVectorMeshFieldPointer_

!----------------------------------------------------------------------------
!                                                 AbstractTensorMeshField_
!----------------------------------------------------------------------------

TYPE, EXTENDS(AbstractMeshField_) :: AbstractTensorMeshField_
END TYPE AbstractTensorMeshField_

!----------------------------------------------------------------------------
!                                           AbstractTensorMeshFieldPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractTensorMeshFieldPointer_
  CLASS(AbstractTensorMeshField_), POINTER :: ptr => NULL()
END TYPE AbstractTensorMeshFieldPointer_

!----------------------------------------------------------------------------
!                                                 AbstractMeshFieldPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractMeshFieldPointer_
  CLASS(AbstractMeshField_), POINTER :: ptr => NULL()
END TYPE AbstractMeshFieldPointer_

!----------------------------------------------------------------------------
!                              SetAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine Check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE SetAbstractMeshFieldParam(param, prefix, name, &
                                fieldType, engine, defineOn, varType, rank, s)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    INTEGER(I4B), INTENT(IN) :: varType
    INTEGER(I4B), INTENT(IN) :: rank
    INTEGER(I4B), INTENT(IN) :: s(:)
  END SUBROUTINE SetAbstractMeshFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine Check the essential parameters in param.

INTERFACE AbstractMeshFieldCheckEssentialParam
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE AbstractMeshFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate the field by reading param and given domain

INTERFACE AbstractMeshFieldInitiate
  MODULE SUBROUTINE obj_Initiate1(obj, param, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE obj_Initiate1
END INTERFACE AbstractMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate by copying other fields, and different options

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
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

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, mesh, material, name, engine, nnt)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(AbstractMaterial_), INTENT(INOUT) :: material
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
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
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Oct 2021
! summary: Deallocates data in [[AbstractMeshField_]]

INTERFACE AbstractMeshFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE AbstractMeshFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(AbstractMeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE AbstractMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE AbstractMeshFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector_Scalar(obj)
    TYPE(AbstractScalarMeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector_Scalar
END INTERFACE AbstractMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE AbstractMeshFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector_Vector(obj)
    TYPE(AbstractVectorMeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector_Vector
END INTERFACE AbstractMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE AbstractMeshFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector_Tensor(obj)
    TYPE(AbstractTensorMeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector_Tensor
END INTERFACE AbstractMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This function returns the size of data

INTERFACE
  MODULE FUNCTION obj_Size(obj, dim) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dim
    INTEGER(I4B) :: ans
    !! if dim is present, then it returns obj%s(dim)
    !! otherwise, it returns the following
    !! for Scalar: return 1
    !! for Vector: return obj%s(1)
    !! for Matrix: return obj%s(1)*obj%s(2)
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Shape@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This function returns the shape of data

INTERFACE
  MODULE FUNCTION obj_Shape(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
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
! summary: Export to hdf5file

INTERFACE
  MODULE SUBROUTINE obj_ExportInVTK(obj, vtk, group)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_ExportInVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Set values in AbstractMeshField_

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalElement, islocal, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), INTENT(IN) :: islocal
    TYPE(FEVariable_), INTENT(IN) :: fevar
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Set values in AbstractMeshField_

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
! summary: Set values in AbstractMeshField_

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: fevar
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Add values to AbstractMeshField_

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

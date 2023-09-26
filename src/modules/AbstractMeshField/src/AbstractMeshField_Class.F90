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
USE GlobalData
USE BaSetype
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE Mesh_Class, ONLY: Mesh_
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class
USE HDF5File_Class
USE VTKFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractMeshField_Class"
PUBLIC :: AbstractMeshField_
PUBLIC :: AbstractMeshFieldPointer_
PUBLIC :: SetAbstractMeshFieldParam
PUBLIC :: AbstractFieldCheckEssentialParam
PUBLIC :: AbstractMeshFieldDeallocate
PUBLIC :: AbstractMeshFieldInitiate
PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                         AbstractMeshField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Abstract node field

TYPE, ABSTRACT :: AbstractMeshField_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is true if the object is initiated
  INTEGER(I4B) :: fieldType = FIELD_TYPE_NORMAL
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
  TYPE(Mesh_), POINTER :: mesh => NULL()
  !! Domain contains the information of the finite element meshes.
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & aField_checkEssentialParam
  !! check essential parameters
  PROCEDURE, PASS(obj) :: Initiate1 => aField_Initiate1
  !! Initiate the field by reading param and a given mesh
  PROCEDURE, PASS(obj) :: Initiate2 => aField_Initiate2
  !! Initiate by copying other fields, and different options
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2
  !! Generic initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => aField_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => aField_Import
  !! Import data from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => aField_Export
  !! Export data in hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ExportInVTK => aField_ExportInVTK
  !! Export data in vtkFile
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => aField_Deallocate
  !! Deallocate the field
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => aField_getPointer
  !! Return pointer to val
  PROCEDURE, PUBLIC, PASS(obj) :: Size => aField_Size
  !! Returns size
  PROCEDURE, PUBLIC, PASS(obj) :: Shape => aField_Shape
  !! Return shape
  PROCEDURE, PUBLIC, PASS(obj) :: Add => aField_Add
  !! Adding a value
  PROCEDURE, PUBLIC, PASS(obj) :: Set => aField_Set
  !! Setting the value
  PROCEDURE, PUBLIC, PASS(obj) :: Get => aField_Get
  !! Getting the value
END TYPE AbstractMeshField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractMeshFieldPointer_
  CLASS(AbstractMeshField_), POINTER :: ptr => NULL()
END TYPE AbstractMeshFieldPointer_

!----------------------------------------------------------------------------
!                              SetAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE SetAbstractMeshFieldParam(param, prefix, name, &
    & fieldType, engine, defineOn, varType, rank, s)
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
!                                     checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE aField_checkEssentialParam(obj, param)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE aField_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE AbstractFieldCheckEssentialParam(obj, prefix, param)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: prefix
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE AbstractFieldCheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Oct 2021
! summary: Deallocates data in [[AbstractMeshField_]]

INTERFACE
  MODULE SUBROUTINE aField_Deallocate(obj)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
  END SUBROUTINE aField_Deallocate
END INTERFACE

INTERFACE AbstractMeshFieldDeallocate
  MODULE PROCEDURE aField_Deallocate
END INTERFACE AbstractMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE aField_Deallocate_Ptr_Vector(obj)
    TYPE(AbstractMeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE aField_Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate the field by reading param and given domain

INTERFACE
  MODULE SUBROUTINE aField_Initiate1(obj, param, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Mesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE aField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate the field by reading param and given domain

INTERFACE
  MODULE SUBROUTINE AbstractMeshFieldInitiate(obj, prefix, param, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: prefix
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Mesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE AbstractMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate by copying other fields, and different options

INTERFACE
  MODULE SUBROUTINE aField_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE aField_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Returns the pointer to a fortran real vector stored inside realVec

INTERFACE
  MODULE FUNCTION aField_GetPointer(obj) RESULT(ans)
    CLASS(AbstractMeshField_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:, :)
  END FUNCTION aField_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Size@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This function returns the size of the field

INTERFACE
  MODULE FUNCTION aField_Size(obj, dim) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dim
    INTEGER(I4B) :: ans
  END FUNCTION aField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Shape@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This function returns the size of the field

INTERFACE
  MODULE FUNCTION aField_Shape(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION aField_Shape
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE aField_Display(obj, msg, unitNo)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE aField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           IMPORT@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Import from hdf5file

INTERFACE
  MODULE SUBROUTINE aField_Import(obj, hdf5, group, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(Mesh_), TARGET, OPTIONAL, INTENT(IN) :: mesh
  END SUBROUTINE aField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Export to hdf5file

INTERFACE
  MODULE SUBROUTINE aField_Export(obj, hdf5, group)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE aField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Export to hdf5file

INTERFACE
  MODULE SUBROUTINE aField_ExportInVTK(obj, vtk, group)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE aField_ExportInVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Export to hdf5file

INTERFACE
  MODULE SUBROUTINE aField_Set(obj, globalElement, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElement
    TYPE(FEVariable_), INTENT(IN) :: fevar
  END SUBROUTINE aField_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Export to hdf5file

INTERFACE
  MODULE SUBROUTINE aField_Add(obj, globalElement, scale, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElement
    REAL(DFP), INTENT(IN) :: scale
    TYPE(FEVariable_), INTENT(IN) :: fevar
  END SUBROUTINE aField_Add
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Export to hdf5file

INTERFACE
  MODULE SUBROUTINE aField_Get(obj, globalElement, fevar)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElement
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
  END SUBROUTINE aField_Get
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractMeshField_Class

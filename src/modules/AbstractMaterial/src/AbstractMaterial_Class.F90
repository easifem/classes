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

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: Abstract class for Material behavior

MODULE AbstractMaterial_Class
USE GlobalData
USE String_Class
USE BaseType
USE UserFunction_Class
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE TxtFile_Class
USE tomlf, ONLY: toml_table
USE tomlf, ONLY: toml_array
USE Fhash, ONLY: FhashTable_ => fhash_tbl_t
IMPLICIT NONE
PRIVATE
PUBLIC :: AbstractMaterial_
PUBLIC :: AbstractMaterialPointer_
PUBLIC :: SetAbstractMaterialParam
PUBLIC :: AbstractMaterialInitiate
PUBLIC :: AbstractMaterialDeallocate
PUBLIC :: AbstractMaterialImport
PUBLIC :: AbstractMaterialExport
PUBLIC :: AbstractMaterialDisplay
PUBLIC :: AbstractMaterialImportFromToml

CHARACTER(*), PARAMETER :: modName = "AbstractMaterial_Class"
CHARACTER(*), PARAMETER :: myprefix = "AbstractMaterial"
REAL(DFP), PARAMETER :: expandScale1 = 2
REAL(DFP), PARAMETER :: expandScale2 = 1.2
INTEGER(I4B), PARAMETER :: thresholdSize = 20
CHARACTER(*), PARAMETER :: toml_mat_prop_name = "property"
!! tomlName.property is the table of table or table which
!! contains the file name, see ImportFromToml

!----------------------------------------------------------------------------
!                                                         AbstractMaterial_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-02
! summary: Abstract Material class

TYPE, ABSTRACT :: AbstractMaterial_
  PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  INTEGER(I4B) :: tProperties = 0_I4B
  !! Total number of properties
  TYPE(String) :: name
    !! name of the material
  TYPE(FhashTable_) :: tbl
  !! Hash table for name to index mapping
  TYPE(UserFunctionPointer_), ALLOCATABLE :: matProps(:)
  !! material properties
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam =>  &
    & obj_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml3 => obj_ImportFromToml3
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2,  &
    & ImportFromToml3

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaterialPointer =>  &
    & obj_GetMaterialPointer
  PROCEDURE, PUBLIC, PASS(obj) :: IsMaterialPresent => obj_IsMaterialPresent

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: AddMaterial1 => obj_AddMaterial1
  PROCEDURE, PASS(obj) :: AddMaterial2 => obj_AddMaterial2
  GENERIC, PUBLIC :: AddMaterial => AddMaterial1, AddMaterial2
  PROCEDURE, PUBLIC, PASS(obj) :: ExpandMatProps => obj_ExpandMatProps
END TYPE AbstractMaterial_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractMaterialPointer_
  CLASS(AbstractMaterial_), POINTER :: ptr => NULL()
END TYPE AbstractMaterialPointer_

!----------------------------------------------------------------------------
!                               SetAbstractMaterialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-08
! update: 2021-12-08
! summary: Sets parameter for abstract material

INTERFACE
  MODULE SUBROUTINE SetAbstractMaterialParam(param, prefix, name)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE SetAbstractMaterialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Check essential parameters

INTERFACE
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(AbstractMaterial_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Initiate the material

INTERFACE AbstractMaterialInitiate
  MODULE SUBROUTINE obj_Initiate(obj, param, prefix)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE obj_Initiate
END INTERFACE AbstractMaterialInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate data

INTERFACE AbstractMaterialDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractMaterialDeallocate

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-18
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractMaterial_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                    AddMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Add material

INTERFACE
  MODULE SUBROUTINE obj_AddMaterial1(obj, name)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_AddMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     AddMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Add material

INTERFACE
  MODULE SUBROUTINE obj_AddMaterial2(obj, name)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: name(:)
  END SUBROUTINE obj_AddMaterial2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ExpandMatProps@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_ExpandMatProps(obj)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_ExpandMatProps
END INTERFACE

!----------------------------------------------------------------------------
!                                               IsMaterialPresent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Add material

INTERFACE
  MODULE FUNCTION obj_IsMaterialPresent(obj, name) RESULT(ans)
    CLASS(AbstractMaterial_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: name
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsMaterialPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMaterial@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-22
! summary:  Get material

INTERFACE
  MODULE FUNCTION obj_GetMaterialPointer(obj, name) RESULT(matPtr)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    TYPE(UserFunction_), POINTER :: matPtr
  END FUNCTION obj_GetMaterialPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractMaterialImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE AbstractMaterialImport

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractMaterialExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractMaterial_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE AbstractMaterialExport

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE AbstractMaterialDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE AbstractMaterialDisplay

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE AbstractMaterialImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE AbstractMaterialImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE AbstractMaterialImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml2(obj, array)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    TYPE(toml_array), POINTER, INTENT(INOUT) :: array
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE AbstractMaterialImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE AbstractMaterialImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml3(obj, tomlName, afile, filename,  &
    & printToml)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml3
END INTERFACE AbstractMaterialImportFromToml

END MODULE AbstractMaterial_Class

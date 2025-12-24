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
USE GlobalData, ONLY: I4B, DFP, LGT
USE String_Class, ONLY: String
USE UserFunction_Class, ONLY: UserFunction_, &
                              UserFunctionPointer_
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table
USE HashTables, ONLY: HashTable_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE MeshSelection_Class, ONLY: MeshSelection_

IMPLICIT NONE

PRIVATE
PUBLIC :: AbstractMaterial_
PUBLIC :: AbstractMaterialPointer_
PUBLIC :: AbstractMaterialInitiate
PUBLIC :: AbstractMaterialDeallocate
PUBLIC :: AbstractMaterialImport
PUBLIC :: AbstractMaterialExport
PUBLIC :: AbstractMaterialDisplay
PUBLIC :: AbstractMaterialImportFromToml
PUBLIC :: TypeMaterial

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMaterial_Class"
#endif

REAL(DFP), PARAMETER :: expandScale1 = 2
REAL(DFP), PARAMETER :: expandScale2 = 1.2
INTEGER(I4B), PARAMETER :: thresholdSize = 20
CHARACTER(*), PARAMETER :: toml_mat_prop_name = "property"
!! tomlName.property is the table of table or table which
!! contains the file name, see ImportFromToml

!----------------------------------------------------------------------------
!                                                          TypeMaterial_
!----------------------------------------------------------------------------

TYPE :: TypeMaterial_
  CHARACTER(5) :: solid = "SOLID"
  CHARACTER(5) :: fluid = "FLUID"
END TYPE TypeMaterial_

TYPE(TypeMaterial_), PARAMETER :: TypeMaterial = TypeMaterial_()

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
  TYPE(HashTable_) :: tbl
  !! Hash table for name to index mapping
  TYPE(UserFunctionPointer_), ALLOCATABLE :: matProps(:)
  !! material properties
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! IO:
  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import

  ! IO:
  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaterialPointer => &
    obj_GetMaterialPointer
  PROCEDURE, PUBLIC, PASS(obj) :: IsMaterialPresent => obj_IsMaterialPresent

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: AddMaterial1 => obj_AddMaterial1
  PROCEDURE, PASS(obj) :: AddMaterial2 => obj_AddMaterial2
  GENERIC, PUBLIC :: AddMaterial => AddMaterial1, AddMaterial2
  PROCEDURE, PUBLIC, PASS(obj) :: ExpandMatProps => obj_ExpandMatProps
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => obj_SetName
  !! Set the name of the material
END TYPE AbstractMaterial_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractMaterialPointer_
  CLASS(AbstractMaterial_), POINTER :: ptr => NULL()
END TYPE AbstractMaterialPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Initiate the material

INTERFACE AbstractMaterialInitiate
  MODULE SUBROUTINE obj_Initiate(obj, name)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_Initiate
END INTERFACE AbstractMaterialInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE AbstractMaterialDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE AbstractMaterialDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary:  Deallocate vector of AbstractMaterial_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    CLASS(AbstractMaterial_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE

INTERFACE AbstractMaterialDeallocate
  MODULE PROCEDURE obj_Deallocate_Vector
END INTERFACE AbstractMaterialDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary:  Deallocate vector of DirichletBCPointer_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(AbstractMaterialPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE

INTERFACE AbstractMaterialDeallocate
  MODULE PROCEDURE obj_Deallocate_Ptr_Vector
END INTERFACE AbstractMaterialDeallocate

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
!                                                         SetName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-14
! summary:  Set the name of the material

INTERFACE
  MODULE SUBROUTINE obj_SetName(obj, name)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  !! Abstract Material object
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetName
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
!                                                           Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary: Import material from HDF5 file

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

INTERFACE AbstractMaterialImport
  MODULE PROCEDURE obj_Import
END INTERFACE AbstractMaterialImport

!----------------------------------------------------------------------------
!                                                          Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary: Export material to HDF5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractMaterial_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

INTERFACE AbstractMaterialExport
  MODULE PROCEDURE obj_Export
END INTERFACE AbstractMaterialExport

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary: Display material information

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE AbstractMaterialDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE AbstractMaterialDisplay

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate AbstractMaterial_ from the toml file
!
!# Introduction
!  This interface is used to import material properties from a toml file.
! If region is given, then we can import the region (MeshSelection_)
! Domain is needed for calling importFromToml on region.
! See MeshSelection_ ImportFromToml for more details.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, region, dom)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(MeshSelection_), OPTIONAL, INTENT(INOUT) :: region
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE AbstractMaterialImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE AbstractMaterialImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml, region, dom)
    CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    TYPE(MeshSelection_), OPTIONAL, INTENT(INOUT) :: region
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE AbstractMaterialImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE AbstractMaterialImportFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractMaterial_Class

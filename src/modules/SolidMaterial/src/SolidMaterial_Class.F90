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
! date: 27 Aug 2021
! summary: This module defines a class called [[SolidMaterial_]]
!
!# Introduction
!
! This module defines a class called [[SolidMaterial_]], which defines a
! solid material and its behavior. Other than defining the class,
! this module makes a routine called
! [[SolidMaterial_Class:SetSolidMaterialParam]] public.
! This routine can be used for setting the options in [[ParameterList_]]
! object.

MODULE SolidMaterial_Class
USE GlobalData, ONLY: I4B, LGT, DFP
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE FPL, ONLY: ParameterList_
USE AbstractMaterial_Class, ONLY: AbstractMaterial_
USE AbstractSolidMechanicsModel_Class, ONLY: AbstractSolidMechanicsModel_
USE MeshSelection_Class, ONLY: MeshSelectionPointer_, MeshSelection_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "SolidMaterial_Class"
CHARACTER(*), PARAMETER :: myprefix = "SolidMaterial"

PUBLIC :: SolidMaterial_
PUBLIC :: SolidMaterialPointer_
PUBLIC :: SolidMaterialDeallocate
PUBLIC :: SolidMaterialReallocate
PUBLIC :: SetSolidMaterialParam
PUBLIC :: AddSolidMaterial
PUBLIC :: GetSolidMaterialPointer
PUBLIC :: TypeSolidMaterial
PUBLIC :: SolidMaterialImportFromToml
PUBLIC :: SolidMaterialNamesFromToml

!----------------------------------------------------------------------------
!                                                            SolidMaterial_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Oct 2021
! summary: SolidMaterial class for material modeling of solids
!
!# Introduction
! SolidMaterial class is a child of [[AbstractMaterial_]].
! It is used for modeling the behavior of solids.

TYPE, EXTENDS(AbstractMaterial_) :: SolidMaterial_
  CLASS(AbstractSolidMechanicsModel_), POINTER :: stressStrainModel => NULL()
    !! Pointer to stress strain material behavior of solids
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    obj_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! IO:
  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetStressStrainModelPointer => &
    obj_GetStressStrainModelPointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
END TYPE SolidMaterial_

!----------------------------------------------------------------------------
!                                                       TypeSolidMaterial
!----------------------------------------------------------------------------

TYPE(SolidMaterial_), PARAMETER :: TypeSolidMaterial = SolidMaterial_()

!----------------------------------------------------------------------------
!                                                     SolidMaterialPointer_
!----------------------------------------------------------------------------

TYPE :: SolidMaterialPointer_
  CLASS(SolidMaterial_), POINTER :: ptr => NULL()
END TYPE SolidMaterialPointer_

!----------------------------------------------------------------------------
!                                  SetSolidMaterialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine sets the essential parameter for [[SolidMaterial_]]
!
!# Introduction
!
! This routine sets the essential parameter for [[SolidMaterial_]].
! It sets values for
!
! - `SolidMaterial/name`
! - `SolidMaterial/massDensity`
! - `SolidMaterial/stresStrainModel`

INTERFACE
  MODULE SUBROUTINE SetSolidMaterialParam(param, name, stressStrainModel)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    !! It is the name of the material
    CHARACTER(*), OPTIONAL, INTENT(IN) :: stressStrainModel
    !! Name of the child-class of `AbstractSolidMechanicsModel_`
    !! For example `LinearElasticModel`
  END SUBROUTINE SetSolidMaterialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine Checks the essential parameter for [[SolidMaterial_]]
!
!# Introduction
!
! This routine Checks the essential parameter for [[SolidMaterial_]].
! It Checks the existance of
!
! - `SolidMaterial/name`
! - `SolidMaterial/stresStrainModel`

INTERFACE
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(SolidMaterial_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the instance of `SolidMaterial`
!
!# Introduction
! This routine initiates the instance of `SolidMaterial_`.
! It reads the options from `param`, and sets the options of `SolidMaterial`
!
!- `SolidMaterial/name`
!- `SolidMaterial/stresStrainModel`
!
! This routine calls the [[MaterialFactory:SolidMechanicsModelFactory]] to
! construct the [[SolidMaterial_:stressStrainModel]].
!
!@warning
! If the `SolidMaterial/stressStrainModel` is already associated, then
! the routine will produce error, so make sure the
! [[SolidMaterial_:stressStrainModel]] is nullified before calling it.
!@endwarning

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param, prefix)
    CLASS(SolidMaterial_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine deallocates the instance
!
!# Introduction
!
! This routine deallocates the memory allocated to the instance of
! `SolidMaterial_`.
!
!@warning
! This routine also deallocates [[SolidMaterial_:stressStrainModel]], if
! it is associated.
!@endwarning

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(SolidMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE SolidMaterialDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE SolidMaterialDeallocate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector

INTERFACE
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(SolidMaterial_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE

INTERFACE SolidMaterialDeallocate
  MODULE PROCEDURE Deallocate_Vector
END INTERFACE SolidMaterialDeallocate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of pointer

INTERFACE
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(SolidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE

INTERFACE SolidMaterialDeallocate
  MODULE PROCEDURE Deallocate_Ptr_Vector
END INTERFACE SolidMaterialDeallocate

!----------------------------------------------------------------------------
!                                              Reallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Reallocate the vector

INTERFACE
  MODULE SUBROUTINE Reallocate_Vector(obj, tsize)
    TYPE(SolidMaterial_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tsize
  END SUBROUTINE Reallocate_Vector
END INTERFACE

INTERFACE SolidMaterialReallocate
  MODULE PROCEDURE Reallocate_Vector
END INTERFACE SolidMaterialReallocate

!----------------------------------------------------------------------------
!                                              Reallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Reallocate the vector of pointer

INTERFACE
  MODULE SUBROUTINE Reallocate_Ptr_Vector(obj, tsize)
    TYPE(SolidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tsize
  END SUBROUTINE Reallocate_Ptr_Vector
END INTERFACE

INTERFACE SolidMaterialReallocate
  MODULE PROCEDURE Reallocate_Ptr_Vector
END INTERFACE SolidMaterialReallocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine deallocates the instance

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(SolidMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                 AddSolidMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-11
! summary:  Add a solid material to the vector of SolidMaterialPointer_

INTERFACE
  MODULE SUBROUTINE obj_AddSolidMaterial(obj, tMaterials, materialNo, &
                                         materialName, solidMaterialToMesh, &
                                         param, region)
    TYPE(SolidMaterialPointer_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tMaterials
    INTEGER(I4B), INTENT(IN) :: materialNo
    CHARACTER(*), OPTIONAL, INTENT(IN) :: materialName
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
    TYPE(MeshSelection_), OPTIONAL, INTENT(IN) :: region
    TYPE(MeshSelection_), OPTIONAL, INTENT(INOUT) :: solidMaterialToMesh(:)
  END SUBROUTINE obj_AddSolidMaterial
END INTERFACE

INTERFACE AddSolidMaterial
  MODULE PROCEDURE obj_AddSolidMaterial
END INTERFACE AddSolidMaterial

!----------------------------------------------------------------------------
!                                         GetSolidMaterialPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-08
! summary:  Get a solid material pointer

INTERFACE
  MODULE FUNCTION obj_GetSolidMaterialPointer(obj, materialNo) RESULT(ans)
    TYPE(SolidMaterialPointer_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: materialNo
    CLASS(SolidMaterial_), POINTER :: ans
  END FUNCTION obj_GetSolidMaterialPointer
END INTERFACE

INTERFACE GetSolidMaterialPointer
  MODULE PROCEDURE obj_GetSolidMaterialPointer
END INTERFACE GetSolidMaterialPointer

!----------------------------------------------------------------------------
!                                     GetStressStrainModelPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-10
! summary: Get stressStrainModel pointer

INTERFACE
  MODULE FUNCTION obj_GetStressStrainModelPointer(obj) RESULT(ans)
    CLASS(SolidMaterial_), INTENT(IN) :: obj
    CLASS(AbstractSolidMechanicsModel_), POINTER :: ans
  END FUNCTION obj_GetStressStrainModelPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(SolidMaterial_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the instance from hdf5 file
!
!# Introduction
! This routine also deallocates the data associated
! summary: This routine initiates the instance from hdf5 file
!

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(SolidMaterial_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine exports the information to external hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(SolidMaterial_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine displays the content of the instance

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(SolidMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate SolidMaterial_ from the toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, region, dom)
    CLASS(SolidMaterial_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(MeshSelection_), OPTIONAL, INTENT(INOUT) :: region
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml table
!
!# Introduction
!
!  This routine initiates several SolidMaterialPointer_ objects
!  from the array of toml table
!  If the array of toml is not found then the size of obj will be set to 0

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, table, materialNames, tsize, &
                                        region, dom)
    TYPE(SolidMaterialPointer_), INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    !! The size should be atleast size of materialNames
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    TYPE(String), INTENT(IN) :: materialNames(:)
    !! Material names that will be read from the toml table
    !! It should be provided by the user
    !! In the table we will look for nodes with these names
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! How many material read from the toml table
    !! If tSize equals to the size of materialNames, then
    !! all the materials are read from the toml table
    !! Otherwise size(materialNames) - tsize materials are not
    !! read successfully from the toml table
    TYPE(MeshSelectionPointer_), OPTIONAL, INTENT(INOUT) :: region(:)
    !! It should be allocated outside
    !! The size of regions should be alteast size of materialNames
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
    !! Domain to which the materials belong
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE SolidMaterialImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE SolidMaterialImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml3(obj, tomlName, afile, filename, &
                                        printToml, tsize, region, dom)
    TYPE(SolidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    CHARACTER(*), INTENT(IN) :: tomlName
    !! tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! Text file to read the toml file
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! Name of the toml file
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! See docs of `obj_ImportFromToml2`
    TYPE(MeshSelectionPointer_), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: &
      region(:)
    !! It should be allocated outside
    !! The size of region should be alteast size of materialNames
    CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom
    !! Domain to which the materials belong
  END SUBROUTINE obj_ImportFromToml3
END INTERFACE

INTERFACE SolidMaterialImportFromToml
  MODULE PROCEDURE obj_ImportFromToml3
END INTERFACE SolidMaterialImportFromToml

!----------------------------------------------------------------------------
!                                              ReadSolidMaterialNamesFromToml
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary:  Read solidMaterialNames from the toml table
!
!# Introduction
!
! This routine reads the solidMaterialNames (a String) from the toml table
! It will reallocate materialNames and return the size of it in tsize.
!
! Then you can allocate a vector of SolidMaterialPointer_ with tsize
! Then you can call SolidMaterialImportFromToml to read the materials
!
! Your toml table looks something like the following (For details
! see SolidMaterial2.toml):
!
! ```toml
! solidMaterialNames = ['solid1', 'solid2', 'solid3']
! # other data
! ```
!
! To read SolidMaterial2.toml you will do the following tasks:
!
! 1. CALL ReadSolidMaterialNamesFromToml(table, materialNames, tsize)
! 2. ALLOCATE(obj(tsize), region(tsize))
! 3. CALL SolidMaterialImportFromToml(obj,table,materialNames,tsize,&
!                                     region,dom)

INTERFACE
  MODULE SUBROUTINE SolidMaterialNamesFromToml(table, materialNames, &
                                                   tsize)
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: materialNames(:)
    !! materialNames to be read from the toml table
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of the materialNames
  END SUBROUTINE SolidMaterialNamesFromToml
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SolidMaterial_Class

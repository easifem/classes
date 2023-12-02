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
USE GlobalData
USE String_Class
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractMaterial_Class
USE AbstractSolidMechanicsModel_Class
USE MeshSelection_Class
USE tomlf, ONLY: toml_table, toml_array
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "SolidMaterial_Class"
CHARACTER(*), PARAMETER :: myprefix = "SolidMaterial"
CHARACTER(*), PARAMETER :: default_stress_strain_toml = "stressStrainModel"
PUBLIC :: SolidMaterial_
PUBLIC :: SolidMaterialPointer_
PUBLIC :: DEALLOCATE
PUBLIC :: SetSolidMaterialParam
PUBLIC :: AddSolidMaterial
PUBLIC :: TypeSolidMaterial

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
    & obj_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetStressStrainModelPointer => &
    & obj_GetStressStrainModelPointer
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
!                                        AddSolidMaterial@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-11
! summary:  Add a solid material to the vector of SolidMaterialPointer_

INTERFACE AddSolidMaterial
  MODULE SUBROUTINE obj_AddSolidMaterial( &
    & obj, &
    & tMaterials,   &
    & materialNo, &
    & materialName,  &
    & solidMaterialToMesh, &
    & param, &
    & region)
    TYPE(SolidMaterialPointer_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tMaterials
    INTEGER(I4B), INTENT(IN) :: materialNo
    CHARACTER(*), OPTIONAL, INTENT(IN) :: materialName
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
    TYPE(MeshSelection_), OPTIONAL, INTENT(IN) :: region
    TYPE(MeshSelection_), OPTIONAL, INTENT(INOUT) :: solidMaterialToMesh(:)
  END SUBROUTINE obj_AddSolidMaterial
END INTERFACE AddSolidMaterial

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
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

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector

INTERFACE DEALLOCATE
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(SolidMaterial_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of pointer

INTERFACE DEALLOCATE
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(SolidMaterialPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                          Final@ConstructorMethods
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
!                                    GetStressStrainModelPointer@GetMethods
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
!                                                          Import@IOMethods
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
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(SolidMaterial_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SolidMaterial_Class

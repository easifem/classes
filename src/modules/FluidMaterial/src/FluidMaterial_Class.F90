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
! date: 2 Oct 2021
! summary: This module defines a class called [[FluidMaterial_]]
!
!# Introduction
!
! This module defines a class called [[FluidMaterial_]], which
! defines a fluid material and its behavior.
! Other than defining the class, this module makes a routine called
! [[FluidMaterial_Class:SetFluidMaterialParam]] public. This routine
! can be used for Setting the options in [[ParameterList_]] object.
!

MODULE FluidMaterial_Class
USE GlobalData, ONLY: I4B, LGT, DFP
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE AbstractMaterial_Class, ONLY: AbstractMaterial_
USE AbstractFluidMechanicsModel_Class, ONLY: AbstractFluidMechanicsModel_
USE MeshSelection_Class, ONLY: MeshSelectionPointer_, MeshSelection_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE

PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "FluidMaterial_Class"
#endif

PUBLIC :: FluidMaterial_
PUBLIC :: FluidMaterialPointer_
PUBLIC :: FluidMaterialDeallocate
PUBLIC :: FluidMaterialReallocate
PUBLIC :: AddFluidMaterial
PUBLIC :: GetFluidMaterialPointer
PUBLIC :: TypeFluidMaterial
PUBLIC :: FluidMaterialImportFromToml
PUBLIC :: FluidMaterialNamesFromToml

!----------------------------------------------------------------------------
!                                                            FluidMaterial_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Oct 2021
! summary: FluidMaterial class for material modeling of Fluids
!
!# Introduction
! FluidMaterial class is a child of [[AbstractMaterial_]].
! It is used for modeling the behavior of Fluids.

TYPE, EXTENDS(AbstractMaterial_) :: FluidMaterial_
  CLASS(AbstractFluidMechanicsModel_), POINTER :: stressStrainModel => NULL()
    !! Pointer to stress strain material behavior of Fluids
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  ! IO:
  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export

  ! IO:
  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetStressStrainModelPointer => &
    obj_GetStressStrainModelPointer
END TYPE FluidMaterial_

!----------------------------------------------------------------------------
!                                                       TypeFluidMaterial
!----------------------------------------------------------------------------

TYPE(FluidMaterial_), PARAMETER :: TypeFluidMaterial = FluidMaterial_()

!----------------------------------------------------------------------------
!                                                     FluidMaterialPointer_
!----------------------------------------------------------------------------

TYPE :: FluidMaterialPointer_
  CLASS(FluidMaterial_), POINTER :: ptr => NULL()
END TYPE FluidMaterialPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-24
! summary: This routine initiates the instance of `FluidMaterial`
!
!# Introduction
! This routine initiates the instance of `FluidMaterial_`.
! It reads the arguments and sets the options of `FluidMaterial`
!
!- `FluidMaterial/name`
!- `FluidMaterial/stresStrainModel`
!
! This routine calls the [[MaterialFactory:FluidMechanicsModelFactory]] to
! construct the [[FluidMaterial_:stressStrainModel]].
!
! If the `FluidMaterial/stressStrainModel` is already associated, then
! the routine will produce error, so make sure the
! [[FluidMaterial_:stressStrainModel]] is nullified before calling it.
!
! This method does not initiate the stressStrainModel,
! after this method call, user should get the pointer of
! stressStrainModel and call Initiate method on it.

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, name, stressStrainModel)
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    !! It is the name of the material
    CHARACTER(*), OPTIONAL, INTENT(IN) :: stressStrainModel
    !! Name of the child-class of `AbstractFluidMechanicsModel_`
    !! For example `LinearElasticModel`
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
! `FluidMaterial_`.
!
!@warning
! This routine also deallocates [[FluidMaterial_:stressStrainModel]], if
! it is associated.
!@endwarning

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE FluidMaterialDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE FluidMaterialDeallocate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector

INTERFACE
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(FluidMaterial_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE

INTERFACE FluidMaterialDeallocate
  MODULE PROCEDURE Deallocate_Vector
END INTERFACE FluidMaterialDeallocate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of pointer

INTERFACE
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(FluidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE

INTERFACE FluidMaterialDeallocate
  MODULE PROCEDURE Deallocate_Ptr_Vector
END INTERFACE FluidMaterialDeallocate

!----------------------------------------------------------------------------
!                                              Reallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Reallocate the vector

INTERFACE
  MODULE SUBROUTINE Reallocate_Vector(obj, tsize)
    TYPE(FluidMaterial_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tsize
  END SUBROUTINE Reallocate_Vector
END INTERFACE

INTERFACE FluidMaterialReallocate
  MODULE PROCEDURE Reallocate_Vector
END INTERFACE FluidMaterialReallocate

!----------------------------------------------------------------------------
!                                              Reallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Reallocate the vector of pointer

INTERFACE
  MODULE SUBROUTINE Reallocate_Ptr_Vector(obj, tsize)
    TYPE(FluidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tsize
  END SUBROUTINE Reallocate_Ptr_Vector
END INTERFACE

INTERFACE FluidMaterialReallocate
  MODULE PROCEDURE Reallocate_Ptr_Vector
END INTERFACE FluidMaterialReallocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine deallocates the instance

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(FluidMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                 AddFluidMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-24
! summary:  Add a Fluid material to the vector of FluidMaterialPointer_
!
!# Introduction
!
! This routine adds a Fluid material to the vector of FluidMaterialPointer_.
! It uses the [[MaterialFactory:FluidMaterialFactory]] to get the
! correct FluidMaterial_ pointer and set it to
! obj(materialNo)%ptr.
! If region and FluidMaterialToMesh are provided,
! then it sets FluidMaterialToMesh(materialNo) = region.
!
! Note that this method will not initiate obj(materialNo)%ptr
! After this call user has to call Initiate method on it

INTERFACE
  MODULE SUBROUTINE obj_AddFluidMaterial( &
    obj, tMaterials, materialNo, materialName, FluidMaterialToMesh, &
    region)
    TYPE(FluidMaterialPointer_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: tMaterials
    INTEGER(I4B), INTENT(IN) :: materialNo
    CHARACTER(*), OPTIONAL, INTENT(IN) :: materialName
    !! Name of the material
    !! If it is provided then we call FluidMaterialFactory to
    !! get a correct FluidMaterial_ pointer and
    !! set it to obj(materialNo)%ptr
    TYPE(MeshSelection_), OPTIONAL, INTENT(IN) :: region
    !! If region is present then it is set to the FluidMaterialToMesh
    TYPE(MeshSelection_), OPTIONAL, INTENT(INOUT) :: FluidMaterialToMesh(:)
    ! If FluidMaterialToMesh is provided, then we check if region is present.
    ! If both are present, the we set
    ! FluidMaterialToMesh(materialNo) = region
  END SUBROUTINE obj_AddFluidMaterial
END INTERFACE

INTERFACE AddFluidMaterial
  MODULE PROCEDURE obj_AddFluidMaterial
END INTERFACE AddFluidMaterial

!----------------------------------------------------------------------------
!                                         GetFluidMaterialPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-08
! summary:  Get a Fluid material pointer

INTERFACE
  MODULE FUNCTION obj_GetFluidMaterialPointer(obj, materialNo) RESULT(ans)
    TYPE(FluidMaterialPointer_), INTENT(INOUT) :: obj(:)
    INTEGER(I4B), INTENT(IN) :: materialNo
    CLASS(FluidMaterial_), POINTER :: ans
  END FUNCTION obj_GetFluidMaterialPointer
END INTERFACE

INTERFACE GetFluidMaterialPointer
  MODULE PROCEDURE obj_GetFluidMaterialPointer
END INTERFACE GetFluidMaterialPointer

!----------------------------------------------------------------------------
!                                     GetStressStrainModelPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-10
! summary: Get stressStrainModel pointer

INTERFACE
  MODULE FUNCTION obj_GetStressStrainModelPointer(obj) RESULT(ans)
    CLASS(FluidMaterial_), INTENT(IN) :: obj
    CLASS(AbstractFluidMechanicsModel_), POINTER :: ans
  END FUNCTION obj_GetStressStrainModelPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@HDFMethods
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
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@HDFMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine exports the information to external hdf5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(FluidMaterial_), INTENT(IN) :: obj
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
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate FluidMaterial_ from the toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, region, dom)
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
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
!  This routine initiates several FluidMaterialPointer_ objects
!  from the array of toml table
!  If the array of toml is not found then the size of obj will be set to 0

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, table, materialNames, tsize, &
                                        region, dom)
    TYPE(FluidMaterialPointer_), INTENT(INOUT) :: obj(:)
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

INTERFACE FluidMaterialImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE FluidMaterialImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml3(obj, tomlName, afile, filename, &
                                        printToml, tsize, region, dom)
    TYPE(FluidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
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

INTERFACE FluidMaterialImportFromToml
  MODULE PROCEDURE obj_ImportFromToml3
END INTERFACE FluidMaterialImportFromToml

!----------------------------------------------------------------------------
!                                              ReadFluidMaterialNamesFromToml
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary:  Read FluidMaterialNames from the toml table
!
!# Introduction
!
! This routine reads the FluidMaterialNames (a String) from the toml table
! It will reallocate materialNames and return the size of it in tsize.
!
! Then you can allocate a vector of FluidMaterialPointer_ with tsize
! Then you can call FluidMaterialImportFromToml to read the materials
!
! Your toml table looks something like the following (For details
! see FluidMaterial2.toml):
!
! ```toml
! FluidMaterialNames = ['Fluid1', 'Fluid2', 'Fluid3']
! # other data
! ```
!
! To read FluidMaterial2.toml you will do the following tasks:
!
! 1. CALL ReadFluidMaterialNamesFromToml(table, materialNames, tsize)
! 2. ALLOCATE(obj(tsize), region(tsize))
! 3. CALL FluidMaterialImportFromToml(obj,table,materialNames,tsize,&
!                                     region,dom)

INTERFACE
  MODULE SUBROUTINE FluidMaterialNamesFromToml(table, materialNames, &
                                               tsize)
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: materialNames(:)
    !! materialNames to be read from the toml table
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of the materialNames
  END SUBROUTINE FluidMaterialNamesFromToml
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FluidMaterial_Class

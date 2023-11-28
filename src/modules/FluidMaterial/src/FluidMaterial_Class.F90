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
USE GlobalData
USE String_Class
USE BaSetype
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractMaterial_Class
USE AbstractFluidMechanicsModel_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "FluidMaterial_Class"
CHARACTER(*), PARAMETER :: myprefix = "FluidMaterial"
PUBLIC :: FluidMaterial_
PUBLIC :: TypeFluidMaterial
PUBLIC :: FluidMaterialPointer_
PUBLIC :: SetFluidMaterialParam

!----------------------------------------------------------------------------
!                                                            FluidMaterial_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Oct 2021
! summary: This data type define a fluid material and its behavior

TYPE, EXTENDS(AbstractMaterial_) :: FluidMaterial_
  CLASS(AbstractFluidMechanicsModel_), POINTER :: stressStrainModel => NULL()
    !! Pointer to stress strain material behavior of fluid
    !! It defines the rheology of the fluid.
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & fluid_CheckEssentialParam
    !! Check the essential parameter
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => fluid_Initiate
    !! Initiate an instance of [[FluidMaterial_]]
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => fluid_Deallocate
    !! Deallocate the memory occupied by the instance of [[FluidMaterial_]]
  FINAL :: fluid_Final
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => fluid_Import
    !! Initiate the instance by importing data from [[HDF5File_]] object
  PROCEDURE, PUBLIC, PASS(obj) :: Export => fluid_Export
    !! Export the data of [[FluidMaterial_]] to [[HDF5File_]] object
  PROCEDURE, PUBLIC, PASS(obj) :: Display => fluid_Display
    !! Display the content of [[FluidMaterial_]]
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => fluid_GetPrefix
END TYPE FluidMaterial_

TYPE(FluidMaterial_), PARAMETER :: TypeFluidMaterial = FluidMaterial_()

!----------------------------------------------------------------------------
!                                                     FluidMaterialPointer_
!----------------------------------------------------------------------------

TYPE :: FluidMaterialPointer_
  CLASS(FluidMaterial_), POINTER :: ptr => NULL()
END TYPE FluidMaterialPointer_

!----------------------------------------------------------------------------
!                                  SetFluidMaterialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine Sets options for constructing [[FluidMaterial_]]
!

INTERFACE
  MODULE SUBROUTINE SetFluidMaterialParam(param, name, stressStrainModel)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    !! It is the name of the material
    CHARACTER(*), OPTIONAL, INTENT(IN) :: stressStrainModel
    !! Name of the child-class of `AbstractSolidMechanicsModel_`
    !! For example `LinearElasticModel`
  END SUBROUTINE SetFluidMaterialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine Checks the essential parameter

INTERFACE
  MODULE SUBROUTINE fluid_CheckEssentialParam(obj, param)
    CLASS(FluidMaterial_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE fluid_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the instance of `Solid`

INTERFACE
  MODULE SUBROUTINE fluid_Initiate(obj, param, prefix)
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), OPTIONAL, INTENT(IN) :: prefix
  END SUBROUTINE fluid_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine deallocates the instance

INTERFACE
  MODULE SUBROUTINE fluid_Deallocate(obj)
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE fluid_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine deallocates the instance

INTERFACE
  MODULE SUBROUTINE fluid_Final(obj)
    TYPE(FluidMaterial_), INTENT(INOUT) :: obj
  END SUBROUTINE fluid_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine initiates the instance from hdf5 file

INTERFACE
  MODULE SUBROUTINE fluid_Import(obj, hdf5, group)
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE fluid_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine exports the information to external hdf5 file

INTERFACE
  MODULE SUBROUTINE fluid_Export(obj, hdf5, group)
    CLASS(FluidMaterial_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE fluid_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine displays the content of the instance

INTERFACE
  MODULE SUBROUTINE fluid_Display(obj, msg, unitNo)
    CLASS(FluidMaterial_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE fluid_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION fluid_GetPrefix(obj) RESULT(ans)
    CLASS(FluidMaterial_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION fluid_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FluidMaterial_Class

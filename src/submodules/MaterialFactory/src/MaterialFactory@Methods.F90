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
! date:         25 Aug 2021
! summary: This module is a factory modules for the material module

SUBMODULE(MaterialFactory) Methods
USE easifemMaterials
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                SolidMechanicsModelFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE SolidMechanicsModelFactory
CHARACTER(*), PARAMETER :: myName = "SolidMechanicsModelFactory"
SELECT CASE (TRIM(name))
CASE ("LinearElasticModel", "linearElasticModel")
  ALLOCATE (LinearElasticModel_ :: ans)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Currently LinearElasticModel as SolidMechanicsModel is &
    & avaiable, we are working on others.')
END SELECT
END PROCEDURE SolidMechanicsModelFactory

!----------------------------------------------------------------------------
!                                                FluidMechanicsModelFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE FluidMechanicsModelFactory
CHARACTER(*), PARAMETER :: myName = "FluidMechanicsModelFactory"
SELECT CASE (TRIM(name))
CASE ("NewtonianFluidModel", "newtonianFluidModel")
  ALLOCATE (NewtonianFluidModel_ :: ans)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Unknown fluid mechanica model, currently, easifemMaterials &
    & supports only the NewtonianFluidModel,we are working on others.')
END SELECT
END PROCEDURE FluidMechanicsModelFactory

!----------------------------------------------------------------------------
!                                                PoroMechanicsModelFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE PoroMechanicsModelFactory
CHARACTER(*), PARAMETER :: myName = "PoroMechanicsModelFactory"
SELECT CASE (TRIM(name))
CASE ("LinearPoroElasticModel", "linearPoroElasticModel")
  ALLOCATE (LinearPoroElasticModel_ :: ans)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Currently LinearPoroElasticModel as PoroMechanicsModel_ is &
    & avaiable, we are working on others.')
END SELECT
END PROCEDURE PoroMechanicsModelFactory

!----------------------------------------------------------------------------
!                                                       SolidMaterialFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE SolidMaterialFactory
CHARACTER(*), PARAMETER :: myName = "SolidMaterialFactory"
SELECT CASE (TRIM(name))
CASE ("SolidMaterial", "solidMaterial")
  ALLOCATE (SolidMaterial_ :: ans)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Cannot find the material name. Currently, only SolidMaterial is &
    & avaiable, we are working on others.')
END SELECT
END PROCEDURE SolidMaterialFactory

!----------------------------------------------------------------------------
!                                                       FluidMaterialFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE FluidMaterialFactory
CHARACTER(*), PARAMETER :: myName = "FluidMaterialFactory"
SELECT CASE (TRIM(name))
CASE ("FluidMaterial", "fluidMaterial")
  ALLOCATE (FluidMaterial_ :: ans)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Cannot find the material name. Currently, only fluidMaterial is &
    & avaiable, we are working on others.')
END SELECT
END PROCEDURE FluidMaterialFactory

!----------------------------------------------------------------------------
!                                                      PorousMaterialFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE PorousMaterialFactory
CHARACTER(*), PARAMETER :: myName = "PorousMaterialFactory"
SELECT CASE (TRIM(name))
CASE ("PorousMaterial", "porousMaterial")
  ALLOCATE (PorousMaterial_ :: ans)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Cannot find the material name. Currently, only porousMaterial is &
    & avaiable, we are working on others.')
END SELECT
END PROCEDURE PorousMaterialFactory

END SUBMODULE Methods

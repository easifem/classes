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
USE String_Class
USE BaseMethod
USE easifemMaterials
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                SolidMechanicsModelFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE SolidMechanicsModelFactory
CHARACTER(*), PARAMETER :: myName = "SolidMechanicsModelFactory"
TYPE(String) :: astr
astr = uppercase(name)
SELECT CASE (astr%chars())
CASE ("LINEARELASTICMODEL")
  ALLOCATE (LinearElasticModel_ :: ans)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Currently LinearElasticModel as '//  &
    & 'SolidMechanicsModel is avaiable, '//  &
    & 'we are working on others.')
  ALLOCATE (LinearElasticModel_ :: ans)
  RETURN
END SELECT
astr = ""
END PROCEDURE SolidMechanicsModelFactory

!----------------------------------------------------------------------------
!                                                FluidMechanicsModelFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE FluidMechanicsModelFactory
CHARACTER(*), PARAMETER :: myName = "FluidMechanicsModelFactory"
TYPE(String) :: astr
astr = uppercase(name)
SELECT CASE (astr%chars())
CASE ("NEWTONIANFLUIDMODEL")
  ALLOCATE (NewtonianFluidModel_ :: ans)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Unknown fluid mechanica model, '//  &
    & 'currently, easifemMaterials supports only the '//  &
    & 'NewtonianFluidModel,we are working on others.')
  ALLOCATE (NewtonianFluidModel_ :: ans)
  RETURN
END SELECT
astr = ""
END PROCEDURE FluidMechanicsModelFactory

!----------------------------------------------------------------------------
!                                                PoroMechanicsModelFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE PoroMechanicsModelFactory
CHARACTER(*), PARAMETER :: myName = "PoroMechanicsModelFactory"
TYPE(String) :: astr
astr = uppercase(name)
SELECT CASE (astr%chars())
CASE ("LINEARPOROELASTICMODEL")
  ALLOCATE (LinearPoroElasticModel_ :: ans)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Currently LinearPoroElasticModel '//  &
    & 'as PoroMechanicsModel_ is avaiable, '//  &
    & ' we are working on others.')
  ALLOCATE (LinearPoroElasticModel_ :: ans)
  RETURN
END SELECT
astr = ""
END PROCEDURE PoroMechanicsModelFactory

!----------------------------------------------------------------------------
!                                                       SolidMaterialFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE SolidMaterialFactory
CHARACTER(*), PARAMETER :: myName = "SolidMaterialFactory"
TYPE(String) :: astr
astr = uppercase(name)
SELECT CASE (astr%chars())
CASE ("SOLIDMATERIAL")
  ALLOCATE (SolidMaterial_ :: ans)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Cannot find the material name. '//  &
    & 'Currently, only SolidMaterial is avaiable, '//  &
    & 'we are working on others.')
  RETURN
END SELECT
astr = ""
END PROCEDURE SolidMaterialFactory

!----------------------------------------------------------------------------
!                                                       FluidMaterialFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE FluidMaterialFactory
CHARACTER(*), PARAMETER :: myName = "FluidMaterialFactory"
TYPE(String) :: astr
astr = uppercase(name)
SELECT CASE (astr%chars())
CASE ("FLUIDMATERIAL")
  ALLOCATE (FluidMaterial_ :: ans)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Cannot find the material name. '//  &
    & 'Currently, only fluidMaterial is avaiable, '// &
    & ' we are working on others.')
  ALLOCATE (FluidMaterial_ :: ans)
  RETURN
END SELECT
astr = ""
END PROCEDURE FluidMaterialFactory

!----------------------------------------------------------------------------
!                                                      PorousMaterialFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE PorousMaterialFactory
CHARACTER(*), PARAMETER :: myName = "PorousMaterialFactory"
TYPE(String) :: astr
astr = uppercase(name)
SELECT CASE (astr%chars())
CASE ("POROUSMATERIAL")
  ALLOCATE (PorousMaterial_ :: ans)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Cannot find the material name. '// &
    & 'Currently, only porousMaterial is avaiable, '// &
    & 'we are working on others.')
  ALLOCATE (PorousMaterial_ :: ans)
  RETURN
END SELECT
END PROCEDURE PorousMaterialFactory

END SUBMODULE Methods

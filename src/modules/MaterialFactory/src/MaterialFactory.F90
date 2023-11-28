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

MODULE MaterialFactory
USE AbstractSolidMechanicsModel_Class, ONLY: AbstractSolidMechanicsModel_
USE AbstractPoroMechanicsModel_Class, ONLY: AbstractPoroMechanicsModel_
USE AbstractFluidMechanicsModel_Class, ONLY: AbstractFluidMechanicsModel_
USE SolidMaterial_Class, ONLY: SolidMaterial_
USE FluidMaterial_Class, ONLY: FluidMaterial_
USE PorousMaterial_Class, ONLY: PorousMaterial_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "MaterialFactory"
PUBLIC :: SolidMechanicsModelFactory
PUBLIC :: FluidMechanicsModelFactory
PUBLIC :: PoroMechanicsModelFactory
PUBLIC :: SolidMaterialFactory
PUBLIC :: FluidMaterialFactory
PUBLIC :: PorousMaterialFactory

!----------------------------------------------------------------------------
!                                               SolidMechanicsModelFactory
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION SolidMechanicsModelFactory(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    CLASS(AbstractSolidMechanicsModel_), POINTER :: ans
  END FUNCTION SolidMechanicsModelFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                               FluidMechanicsModelFactory
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION FluidMechanicsModelFactory(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    CLASS(AbstractFluidMechanicsModel_), POINTER :: ans
  END FUNCTION FluidMechanicsModelFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                               PoroMechanicsModelFactory
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION PoroMechanicsModelFactory(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    CLASS(AbstractPoroMechanicsModel_), POINTER :: ans
  END FUNCTION PoroMechanicsModelFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SolidMaterialFactory
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION SolidMaterialFactory(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    CLASS(SolidMaterial_), POINTER :: ans
  END FUNCTION SolidMaterialFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                      FluidMaterialFactory
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION FluidMaterialFactory(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    CLASS(FluidMaterial_), POINTER :: ans
  END FUNCTION FluidMaterialFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                      PorousMaterialFactory
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION PorousMaterialFactory(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    CLASS(PorousMaterial_), POINTER :: ans
  END FUNCTION PorousMaterialFactory
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MaterialFactory

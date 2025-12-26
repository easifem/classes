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
! summary: A module for material behavior of fluids
!
!# Introduction
!
! This module defines an abstract class [[AbstractFluidMechanicsModel_]]
! for modeling the stres strain behavior of fluids.

MODULE AbstractFluidMechanicsModel_Class
USE GlobalData, ONLY: I4B
USE AbstractMaterialModel_Class, ONLY: AbstractMaterialModel_

IMPLICIT NONE

PRIVATE

PUBLIC :: AbstractFluidMechanicsModel_
PUBLIC :: AbstractFluidMechanicsModelPointer_

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractFluidMechanicsModel_Class"
#endif

!----------------------------------------------------------------------------
!                                                      FluidMechanicsModel_
!----------------------------------------------------------------------------

TYPE, ABSTRACT, EXTENDS(AbstractMaterialModel_) :: &
  AbstractFluidMechanicsModel_
END TYPE AbstractFluidMechanicsModel_

!----------------------------------------------------------------------------
!                                         AbstractFluidMechanicsModelPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractFluidMechanicsModelPointer_
  CLASS(AbstractFluidMechanicsModel_), POINTER :: ptr => NULL()
END TYPE AbstractFluidMechanicsModelPointer_

END MODULE AbstractFluidMechanicsModel_Class

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

SUBMODULE(AbstractSolidMechanicsModel_Class) ConstructorMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "lem_CheckEssentialParam"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of'//  &
  & CHAR_LF//'AbstractSolidMechanicsModel_')
END PROCEDURE lem_CheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_Initiate
CHARACTER(*), PARAMETER :: myName = "lem_Initiate"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child of '//  &
  & CHAR_LF//'AbstractSolidMechanicsModel_')
END PROCEDURE lem_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lem_Deallocate
CALL AbstractMaterialModelDeallocate(obj)
obj%isPStress = .FALSE.
obj%isPStrain = .FALSE.
END PROCEDURE lem_Deallocate

END SUBMODULE ConstructorMethods

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

SUBMODULE(AbstractMatrixField_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE amField_Display
CALL AbstractFieldDisplay(obj=obj, msg=msg, unitNo=unitNo)
IF (obj%isPmatInitiated) THEN
  CALL Display("# isPmatInitiated : TRUE ", unitNo)
ELSE
  CALL Display("# isPmatInitiated : FALSE", unitNo)
END IF
END PROCEDURE amField_Display

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE amField_Deallocate
CALL AbstractFieldDeallocate(obj)
obj%isPmatInitiated = .FALSE.
END PROCEDURE amField_Deallocate

!----------------------------------------------------------------------------
!                                                   isPreconditionSet
!----------------------------------------------------------------------------

MODULE PROCEDURE amField_isPreconditionSet
ans = obj%isPmatInitiated
END PROCEDURE amField_isPreconditionSet

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE amField_SPY
CHARACTER(*), PARAMETER :: myName = "amField_SPY"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine has not been implemented by the child class.')
END PROCEDURE amField_SPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE amField_SymSchurLargestEigenVal
CHARACTER(*), PARAMETER :: myName = "amField_SymSchurLargestEigenVal"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine has not been implemented by the child class.')
END PROCEDURE amField_SymSchurLargestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE amField_SymLargestEigenVal
CHARACTER(*), PARAMETER :: myName = "amField_SymLargestEigenVal"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine has not been implemented by the child class.')
END PROCEDURE amField_SymLargestEigenVal

END SUBMODULE Methods

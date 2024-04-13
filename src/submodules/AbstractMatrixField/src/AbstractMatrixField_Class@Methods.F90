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

MODULE PROCEDURE obj_Display
CALL AbstractFieldDisplay(obj=obj, msg=msg, unitNo=unitNo)
IF (obj%isPmatInitiated) THEN
  CALL Display("# isPmatInitiated : TRUE ", unitNo)
ELSE
  CALL Display("# isPmatInitiated : FALSE", unitNo)
END IF
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL AbstractFieldDeallocate(obj)
obj%isPmatInitiated = .FALSE.
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                   isPreconditionSet
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isPreconditionSet
ans = obj%isPmatInitiated
END PROCEDURE obj_isPreconditionSet

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SPY
CHARACTER(*), PARAMETER :: myName = "obj_SPY"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine has not been implemented by the child class.')
END PROCEDURE obj_SPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SymSchurLargestEigenVal
CHARACTER(*), PARAMETER :: myName = "obj_SymSchurLargestEigenVal"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine has not been implemented by the child class.')
END PROCEDURE obj_SymSchurLargestEigenVal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SymLargestEigenVal
CHARACTER(*), PARAMETER :: myName = "obj_SymLargestEigenVal"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine has not been implemented by the child class.')
END PROCEDURE obj_SymLargestEigenVal

END SUBMODULE Methods

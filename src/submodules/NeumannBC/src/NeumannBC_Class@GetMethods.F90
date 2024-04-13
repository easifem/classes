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

SUBMODULE(NeumannBC_Class) GetMethods
USE BaseMethod, ONLY: TOSTRING, Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    GetNeumannBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNeumannBCPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetNeumannBCPointer()"
INTEGER(I4B) :: nbcNo0, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

tsize = SIZE(nbc)

nbcNo0 = Input(default=tsize, option=nbcNo)

#ifdef DEBUG_VER
IF (nbcNo0 .GT. tsize) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
   & '[INTERNAL ERROR] :: nbcNo0 is out of bound for nbc')
END IF

IF (.NOT. ASSOCIATED(nbc(nbcNo0)%ptr)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: nbc( '//TOSTRING(nbcNo0) &
    & //')%ptr is not ASSOCIATED')
END IF
#endif

ans => nbc(nbcNo0)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_GetNeumannBCPointer

!----------------------------------------------------------------------------
!                                                               GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

END SUBMODULE GetMethods

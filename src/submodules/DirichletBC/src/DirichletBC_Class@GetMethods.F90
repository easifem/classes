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

SUBMODULE(DirichletBC_Class) GetMethods
USE BaseMethod, ONLY: TOSTRING, Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    GetDirichletBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCPointer()"
INTEGER(I4B) :: dbcNo0, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

tsize = SIZE(dbc)

dbcNo0 = Input(default=tsize, option=dbcNo)

#ifdef DEBUG_VER
IF (dbcNo0 .GT. tsize) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
   & '[INTERNAL ERROR] :: dbcNo0 is out of bound for dbc')
END IF

IF (.NOT. ASSOCIATED(dbc(dbcNo0)%ptr)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: dbc( '//TOSTRING(dbcNo0) &
    & //')%ptr is not ASSOCIATED')
END IF
#endif

ans => dbc(dbcNo0)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCPointer

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

END SUBMODULE GetMethods

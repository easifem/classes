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
USE BaseMethod, ONLY: ToString, Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    GetDirichletBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCPointer()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: dbcNo0, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(dbc)

dbcNo0 = Input(default=tsize, option=dbcNo)

#ifdef DEBUG_VER
isok = dbcNo0 .LE. tsize
CALL AssertError1(isok, myName, &
      "dbcNo0="//ToString(dbcNo0)//" is out of bound tsize="//ToString(tsize))
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(dbc(dbcNo0)%ptr)
CALL AssertError1(isok, myName, &
                  "dbc("//ToString(dbcNo0)//")%ptr is not ASSOCIATED")
#endif

ans => dbc(dbcNo0)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCPointer

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

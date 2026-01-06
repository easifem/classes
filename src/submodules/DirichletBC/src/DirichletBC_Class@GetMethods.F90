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
#ifdef DEBUG_VER
USE Display_Method, ONLY: ToString
#endif

USE InputUtility, ONLY: Input

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

INTEGER(I4B) :: bcNo0, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(bc)

bcNo0 = Input(default=tsize, option=bcNo)

#ifdef DEBUG_VER
isok = bcNo0 .LE. tsize
CALL AssertError1(isok, myName, &
        "bcNo0="//ToString(bcNo0)//" is out of bound tsize="//ToString(tsize))
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(bc(bcNo0)%ptr)
CALL AssertError1(isok, myName, &
                  "bc("//ToString(bcNo0)//")%ptr is not ASSOCIATED")
#endif

ans => bc(bcNo0)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCPointer

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

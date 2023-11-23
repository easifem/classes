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
USE BaseMethod, ONLY: TOSTRING
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    GetDirichletBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetDirichletBCPointer
CHARACTER(*), PARAMETER :: myName = "bc_GetDirichletBCPointer"

IF (dbcNo .GT. SIZE(dbc)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
   & '[OUT OF BOUND ERROR] :: dbcNo is out of bound for dbc')
END IF

IF (.NOT. ASSOCIATED(dbc(dbcNo)%ptr)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & '[ALLOCATION ERROR] :: dbc( '//TOSTRING(dbcNo) &
    & //')%ptr is not ASSOCIATED')
END IF

ans => dbc(dbcNo)%ptr

END PROCEDURE bc_GetDirichletBCPointer

!----------------------------------------------------------------------------
!                                                                 GetPrefix 
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_GetPrefix
  ans = myprefix
END PROCEDURE bc_GetPrefix

END SUBMODULE GetMethods

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

SUBMODULE(NitscheBC_Class) GetMethods
USE BaseMethod, ONLY: TOSTRING, Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         GetMinCellEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinCellEntity
IF (ALLOCATED(obj%cellEntity)) THEN
  ans = LBOUND(obj%cellEntity, 1)
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetMinCellEntity

!----------------------------------------------------------------------------
!                                                           GetMaxCellEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxCellEntity
IF (ALLOCATED(obj%cellEntity)) THEN
  ans = UBOUND(obj%cellEntity, 1) - 1
ELSE
  ans = 0
END IF
END PROCEDURE obj_GetMaxCellEntity

!----------------------------------------------------------------------------
!                                                        IsCellEntityPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsCellEntityPresent
INTEGER(I4B) :: ii

IF (ALLOCATED(obj%cellEntity)) THEN
  IF (entityNum .LT. SIZE(obj%cellEntity)) THEN
    ii = obj%cellEntity(entityNum + 1) - obj%cellEntity(entityNum)
    IF (ii .GT. 0) THEN
      ans = .TRUE.
    ELSE
      ans = .FALSE.
    END IF
  ELSE
    ans = .FALSE.
  END IF
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_IsCellEntityPresent

!----------------------------------------------------------------------------
!                                                             GetStartIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStartIndex
ans = obj%cellEntity(entityNum)
END PROCEDURE obj_GetStartIndex

!----------------------------------------------------------------------------
!                                                                GetEndIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEndIndex
ans = obj%cellEntity(entityNum + 1) - 1
END PROCEDURE obj_GetEndIndex

!----------------------------------------------------------------------------
!                                                               GetCellElem
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellElem
ans = obj%cellElem(entityNum)
END PROCEDURE obj_GetCellElem

!----------------------------------------------------------------------------
!                                                            GetLocalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetID
ans = obj%localFacetID(entityNum)
END PROCEDURE obj_GetLocalFacetID

!----------------------------------------------------------------------------
!                                                        GetNitscheBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNitscheBCPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetNitscheBCPointer"
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

END PROCEDURE obj_GetNitscheBCPointer

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

END SUBMODULE GetMethods

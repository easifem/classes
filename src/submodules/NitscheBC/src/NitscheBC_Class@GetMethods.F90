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
IMPLICIT NONE
CONTAINS

MODULE PROCEDURE bc_GetMinCellEntity
IF (ALLOCATED(obj%cellEntity)) THEN
  ans = LBOUND(obj%cellEntity, 1)
ELSE
  ans = 0
END IF
END PROCEDURE bc_GetMinCellEntity

MODULE PROCEDURE bc_GetMaxCellEntity
IF (ALLOCATED(obj%cellEntity)) THEN
  ans = UBOUND(obj%cellEntity, 1) - 1
ELSE
  ans = 0
END IF
END PROCEDURE bc_GetMaxCellEntity

MODULE PROCEDURE bc_isCellEntityPresent
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
END PROCEDURE bc_isCellEntityPresent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_getStartIndex
ans = obj%cellEntity(entityNum)
END PROCEDURE bc_getStartIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_getEndIndex
ans = obj%cellEntity(entityNum + 1) - 1
END PROCEDURE bc_getEndIndex

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_getCellElem
ans = obj%cellElem(entityNum)
END PROCEDURE bc_getCellElem

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_getLocalFacetID
ans = obj%localFacetID(entityNum)
END PROCEDURE bc_getLocalFacetID

END SUBMODULE GetMethods

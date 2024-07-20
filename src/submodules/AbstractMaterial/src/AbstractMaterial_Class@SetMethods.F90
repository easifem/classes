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

SUBMODULE(AbstractMaterial_Class) SetMethods
USE BaseMethod
USE HashTables, ONLY: Hashkey
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               AddMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddMaterial1
CHARACTER(*), PARAMETER :: myName = "obj_AddMaterial1()"
LOGICAL(LGT) :: isMatPresent, matPropsAlloc, sizeOK
INTEGER(I4B) :: matPropSize

IF (.NOT. obj%isInit) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: AbstractMaterial_::obj is not initiated.')
  RETURN
END IF

isMatPresent = obj%IsMaterialPresent(name)
IF (isMatPresent) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: The material '//name// &
                    ' is already present!')
  RETURN
END IF

obj%tProperties = obj%tProperties + 1

CALL obj%tbl%set(Hashkey(name), obj%tProperties)

matPropsAlloc = ALLOCATED(obj%matProps)

IF (matPropsAlloc) THEN
  matPropSize = SIZE(obj%matProps)
ELSE
  matPropSize = 0
END IF

sizeOK = obj%tProperties .LE. matPropSize

IF (.NOT. sizeOK) THEN
  CALL obj%ExpandMatProps()
END IF

ALLOCATE (obj%matProps(obj%tProperties)%ptr)

END PROCEDURE obj_AddMaterial1

!----------------------------------------------------------------------------
!                                                                AddMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddMaterial2
INTEGER(I4B) :: ii, tsize
tsize = SIZE(name)
DO ii = 1, tsize
  CALL obj%AddMaterial(name=name(ii)%chars())
END DO
END PROCEDURE obj_AddMaterial2

!----------------------------------------------------------------------------
!                                                           ExpandMatProps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExpandMatProps
TYPE(UserFunctionPointer_), ALLOCATABLE :: temp(:)
INTEGER(I4B) :: oldSize, newSize, ii
LOGICAL(LGT) :: matPropsAlloc

matPropsAlloc = ALLOCATED(obj%matProps)

IF (.NOT. matPropsAlloc) THEN
  ALLOCATE (obj%matProps(thresholdSize))
  RETURN
END IF

oldSize = SIZE(obj%matProps)

IF (oldSize .LE. thresholdSize) THEN
  newSize = INT(expandScale1 * oldSize, kind=I4B)
ELSE
  newSize = INT(expandScale2 * oldSize, kind=I4B)
END IF

ALLOCATE (temp(oldSize))

DO ii = 1, oldSize
  temp(ii)%ptr => obj%matProps(ii)%ptr
  obj%matProps(ii)%ptr => NULL()
END DO

DEALLOCATE (obj%matProps)
ALLOCATE (obj%matProps(newSize))

DO ii = 1, oldSize
  obj%matProps(ii)%ptr => temp(ii)%ptr
  temp(ii)%ptr => NULL()
END DO

DEALLOCATE (temp)

END PROCEDURE obj_ExpandMatProps

END SUBMODULE SetMethods

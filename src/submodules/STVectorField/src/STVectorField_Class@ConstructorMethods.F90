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

SUBMODULE(STVectorField_Class) ConstructorMethods
USE Display_Method, ONLY: ToString
USE FPL_Method, ONLY: Set, GetValue
USE String_Class, ONLY: String
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldSetParam, &
                                   AbstractNodeFieldInitiate, &
                                   AbstractNodeFieldDeallocate
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
USE ArangeUtility, ONLY: Arange

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
INTEGER(I4B) :: ii, tsize

CALL AbstractNodeFieldInitiate( &
  obj=obj, obj2=obj2, copyFull=copyFull, copyStructure=copyStructure, &
  usePointer=usePointer)

SELECT TYPE (obj2); CLASS IS (STVectorField_)
  obj%spaceCompo = obj2%spaceCompo
  obj%timeCompo = obj2%timeCompo

  tsize = SafeSize(obj2%idofs)
  CALL Reallocate(obj%idofs, tsize)
  DO ii = 1, tsize
    obj%idofs(ii) = obj2%idofs(ii)
  END DO

  tsize = SafeSize(obj2%space_idofs)
  CALL Reallocate(obj%space_idofs, tsize)
  DO ii = 1, tsize
    obj%space_idofs(ii) = obj2%space_idofs(ii)
  END DO

  tsize = SafeSize(obj2%time_idofs)
  CALL Reallocate(obj%time_idofs, tsize)
  DO ii = 1, tsize
    obj%time_idofs(ii) = obj2%time_idofs(ii)
  END DO
END SELECT
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%spaceCompo = 0_I4B
obj%timeCompo = 0_I4B
IF (ALLOCATED(obj%idofs)) DEALLOCATE (obj%idofs)
IF (ALLOCATED(obj%space_idofs)) DEALLOCATE (obj%space_idofs)
IF (ALLOCATED(obj%time_idofs)) DEALLOCATE (obj%time_idofs)
CALL AbstractNodeFieldDeallocate(obj)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                           Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                   VectorFieldSafeAllocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_STVectorFieldSafeAllocate1
LOGICAL(LGT) :: isalloc
INTEGER(I4B) :: tsize

isalloc = ALLOCATED(obj)

IF (.NOT. isalloc) THEN
  ALLOCATE (obj(newsize))
  RETURN
END IF

tsize = SIZE(obj)

IF (tsize .LT. newsize) THEN
  CALL STVectorFieldDeallocate(obj)
  ALLOCATE (obj(newsize))
END IF

END PROCEDURE obj_STVectorFieldSafeAllocate1

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods

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

SUBMODULE(NeumannBC_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL obj(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE obj_Deallocate_Vector

!----------------------------------------------------------------------------
!                                                                 Deallocate
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
!                                                               AddNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddNeumannBC
CHARACTER(*), PARAMETER :: myName = "obj_AddNeumannBC"

IF (nbcNo .GT. SIZE(nbc)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[OUT OF BOUND ERROR] :: nbcNo [= '//TOSTRING(nbcNo)//  &
  & '] is out of bound for nbc [= '// &
  & TOSTRING(SIZE(nbc))//']')
END IF

IF (ASSOCIATED(nbc(nbcNo)%ptr)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[ALLOCATION ERROR] :: nbc( '//TOSTRING(nbcNo)// &
  &  ')%ptr is already associated, deallocate and nullify it first.')
END IF

ALLOCATE (nbc(nbcNo)%ptr)

CALL nbc(nbcNo)%ptr%initiate( &
  & param=param, &
  & boundary=boundary, &
  & dom=dom)

END PROCEDURE obj_AddNeumannBC

!----------------------------------------------------------------------------
!                                                                  AppendBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AppendNeumannBC
CHARACTER(*), PARAMETER :: myName = "obj_AppendNeumannBC()"
INTEGER(I4B) :: tsize, ii, nbcNo0
LOGICAL(LGT) :: isExpand
TYPE(NeumannBCPointer_), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (ALLOCATED(nbc)) THEN
  tsize = SIZE(nbc)
ELSE
  tsize = 0
END IF

nbcNo0 = Input(default=tsize + 1, option=nbcNo)

isExpand = nbcNo0 .GT. tsize

IF (isExpand) THEN
  ALLOCATE (temp(tsize))
  DO ii = 1, tsize; temp(ii)%ptr => nbc(ii)%ptr; END DO
  DO ii = 1, tsize; nbc(ii)%ptr => NULL(); END DO
  DEALLOCATE (nbc)
  ALLOCATE (nbc(nbcNo0))
  DO ii = 1, tsize; nbc(ii)%ptr => temp(ii)%ptr; END DO
  DO ii = 1, tsize; temp(ii)%ptr => NULL(); END DO
  DO ii = tsize + 1, nbcNo0; nbc(ii)%ptr => NULL(); END DO
END IF

IF (ASSOCIATED(nbc(nbcNo0)%ptr)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[ALLOCATION ERROR] :: nbc( '//TOSTRING(nbcNo0)// &
  &  ')%ptr is already associated, deallocate and nullify it first.')
END IF

ALLOCATE (nbc(nbcNo0)%ptr)
CALL nbc(nbcNo0)%ptr%initiate(param=param, boundary=boundary, dom=dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_AppendNeumannBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods

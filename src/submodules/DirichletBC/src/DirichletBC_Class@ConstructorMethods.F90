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

SUBMODULE(DirichletBC_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
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
!                                                             Deallocate
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
!                                                            AddDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddDirichletBC
CHARACTER(*), PARAMETER :: myName = "obj_AddDirichletBC"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] AddDirichletBC()')
#endif

IF (dbcNo .GT. SIZE(dbc)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[OUT OF BOUND ERROR] :: dbcNo [= '//TOSTRING(dbcNo)//  &
  & '] is out of bound for dbc [= '// &
  & TOSTRING(SIZE(dbc))//']')
END IF

IF (ASSOCIATED(dbc(dbcNo)%ptr)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[ALLOCATION ERROR] :: DBC( '//TOSTRING(dbcNo)// &
  &  ')%ptr is already associated, deallocate and nullify it first.')
END IF

ALLOCATE (dbc(dbcNo)%ptr)

CALL dbc(dbcNo)%ptr%initiate( &
  & param=param, &
  & boundary=boundary, &
  & dom=dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] AddDirichletBC()')
#endif
END PROCEDURE obj_AddDirichletBC

!----------------------------------------------------------------------------
!                                                           AppendNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AppendDirichletBC
CHARACTER(*), PARAMETER :: myName = "obj_AppendDirichletBC()"
INTEGER(I4B) :: tsize, ii, dbcNo0
LOGICAL(LGT) :: isExpand
TYPE(DirichletBCPointer_), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (ALLOCATED(dbc)) THEN
  tsize = SIZE(dbc)
ELSE
  tsize = 0
END IF

dbcNo0 = Input(default=tsize + 1, option=dbcNo)

isExpand = dbcNo0 .GT. tsize

IF (isExpand) THEN
  ALLOCATE (temp(tsize))
  DO ii = 1, tsize; temp(ii)%ptr => dbc(ii)%ptr; END DO
  DO ii = 1, tsize; dbc(ii)%ptr => NULL(); END DO
  DEALLOCATE (dbc)
  ALLOCATE (dbc(dbcNo0))
  DO ii = 1, tsize; dbc(ii)%ptr => temp(ii)%ptr; END DO
  DO ii = 1, tsize; temp(ii)%ptr => NULL(); END DO
  DO ii = tsize + 1, dbcNo0; dbc(ii)%ptr => NULL(); END DO
END IF

IF (ASSOCIATED(dbc(dbcNo0)%ptr)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[ALLOCATION ERROR] :: dbc( '//TOSTRING(dbcNo0)// &
  &  ')%ptr is already associated, deallocate and nullify it first.')
END IF

ALLOCATE (dbc(dbcNo0)%ptr)
CALL dbc(dbcNo0)%ptr%initiate(param=param, boundary=boundary, dom=dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_AppendDirichletBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods

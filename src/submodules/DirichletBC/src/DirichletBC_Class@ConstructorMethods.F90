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
USE Display_Method, ONLY: Display, ToString
USE InputUtility, ONLY: Input

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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddDirichletBC"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = dbcNo .LE. SIZE(dbc)
CALL AssertError1(isok, myName, &
                  '[OUT OF BOUND ERROR] :: dbcNo [= '//ToString(dbcNo)// &
                  '] is out of bound for dbc [= '// &
                  ToString(SIZE(dbc))//']')
#endif

#ifdef DEBUG_VER
isok = .NOT. ASSOCIATED(dbc(dbcNo)%ptr)
CALL AssertError1(isok, myName, &
                  '[ALLOCATION ERROR] :: DBC( '//ToString(dbcNo)// &
              ')%ptr is already associated, deallocate and nullify it first.')
#endif

ALLOCATE (dbc(dbcNo)%ptr)
CALL dbc(dbcNo)%ptr%Initiate(param=param, boundary=boundary, dom=dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddDirichletBC

!----------------------------------------------------------------------------
!                                                           AppendNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AppendDirichletBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AppendDirichletBC()"
#endif

INTEGER(I4B) :: tsize, ii, dbcNo0
LOGICAL(LGT) :: isExpand, isok
TYPE(DirichletBCPointer_), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0
isok = ALLOCATED(dbc); IF (isok) tsize = SIZE(dbc)

dbcNo0 = Input(default=tsize + 1, option=dbcNo)

isExpand = dbcNo0 .GT. tsize

IF (isExpand) THEN
  ALLOCATE (temp(tsize))
  DO ii = 1, tsize
    temp(ii)%ptr => dbc(ii)%ptr
  END DO

  DO ii = 1, tsize
    dbc(ii)%ptr => NULL()
  END DO

  DEALLOCATE (dbc)
  ALLOCATE (dbc(dbcNo0))

  DO ii = 1, tsize
    dbc(ii)%ptr => temp(ii)%ptr
  END DO

  DO ii = 1, tsize
    temp(ii)%ptr => NULL()
  END DO

  DO ii = tsize + 1, dbcNo0
    dbc(ii)%ptr => NULL()
  END DO
END IF

#ifdef DEBUG_VER
isok = .NOT. ASSOCIATED(dbc(dbcNo0)%ptr)
CALL AssertError1(isok, myName, &
                  '[ALLOCATION ERROR] :: dbc( '//ToString(dbcNo0)// &
              ')%ptr is already associated, deallocate and nullify it first.')
#endif

ALLOCATE (dbc(dbcNo0)%ptr)
CALL dbc(dbcNo0)%ptr%Initiate(param=param, boundary=boundary, dom=dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AppendDirichletBC

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods

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

MODULE PROCEDURE bc_Final
CALL obj%DEALLOCATE()
END PROCEDURE bc_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Deallocate_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL obj(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE bc_Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Deallocate_Ptr_Vector
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
END PROCEDURE bc_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                            AddDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_AddDirichletBC
CHARACTER(*), PARAMETER :: myName = "bc_AddDirichletBC"

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
END PROCEDURE bc_AddDirichletBC

END SUBMODULE ConstructorMethods

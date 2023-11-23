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

MODULE PROCEDURE bc_Final
CALL obj%DEALLOCATE()
END PROCEDURE bc_Final

!----------------------------------------------------------------------------
!                                                                 Deallocate
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
!                                                                 Deallocate
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
!                                                               AddNeumannBC
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_AddNeumannBC
CHARACTER(*), PARAMETER :: myName = "bc_AddNeumannBC"

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

END PROCEDURE bc_AddNeumannBC

END SUBMODULE ConstructorMethods

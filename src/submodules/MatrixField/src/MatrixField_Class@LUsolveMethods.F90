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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains matrix vector method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) LUSolveMethods
USE InputUtility, ONLY: Input

USE CSRMatrix_Method, ONLY: LUSOLVE, LUTSOLVE

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    LUSOLVE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ILUSOLVE1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ILUSOLVE1()"
INTEGER(I4B) :: s(2), info, sol1, rhs1
#endif

LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
                  '[INTERNAL ERROR] :: MatrixField_ object is not initiated.')
  RETURN
END IF

IF (obj%engine%chars() .NE. "NATIVE_SERIAL") THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
        '[INTERNAL ERROR] :: This routine is only avaiable for NATIVE_SERIAL')
  RETURN
END IF

s = obj%SHAPE()
sol1 = SIZE(sol)
rhs1 = SIZE(rhs)

IF (sol1 .NE. rhs1 .OR. sol1 .NE. s(1)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  '[INTERNAL ERROR] :: Size of sol vector should be equal to the size of rhs')
  RETURN
END IF

#endif

abool = Input(default=.FALSE., option=isTranspose)

IF (abool) THEN
  CALL LUTSOLVE(sol=sol, rhs=rhs, alu=obj%pmat%A, jlu=obj%pmat%JA, &
                ju=obj%pmat%JU)
  RETURN
END IF

CALL LUSOLVE(sol=sol, rhs=rhs, alu=obj%pmat%A, jlu=obj%pmat%JA, &
             ju=obj%pmat%JU)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ILUSOLVE1

!----------------------------------------------------------------------------
!                                                                   LUSOLVE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ILUSOLVE2
REAL(DFP), POINTER :: solval(:)
REAL(DFP), POINTER :: rhsval(:)
solval => sol%GetPointer()
rhsval => rhs%GetPointer()
CALL obj%ILUSOLVE(sol=solval, rhs=rhsval, isTranspose=isTranspose)
NULLIFY (solval, rhsval)
END PROCEDURE obj_ILUSOLVE2

END SUBMODULE LUSolveMethods

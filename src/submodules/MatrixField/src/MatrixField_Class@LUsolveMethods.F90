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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    LUSOLVE
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_ILUSOLVE1
CHARACTER(*), PARAMETER :: myName = "mField_ILUSOLVE1"
INTEGER(I4B) :: s(2), info
!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'MatrixField_ object is not initiated.')
END IF
!
IF (.NOT. obj%isPmatInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Pmat is not initiated')
  ! CALL LinSolve( &
  !   & A=obj%mat, &
  !   & B=rhs, &
  !   & X=sol, &
  !   & isTranspose=.FALSE., &
  !   & isFactored=.TRUE., &
  !   & PrintStat=yes_no_t%NO, &
  !   & info=info)
  ! IF (info .NE. 0) THEN
  !   CALL e%raiseError(modName//'::'//myName//' - '// &
  !     & 'Failure in LinSolve()')
  ! END IF
ELSE
  s = obj%SHAPE()

  IF (SIZE(sol) .NE. SIZE(rhs) .OR. SIZE(sol) .NE. s(1)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Size of sol vector should be equal to the size of rhs')
  END IF

  IF (INPUT(default=.FALSE., option=isTranspose)) THEN
    CALL LUTSOLVE( &
      & sol=sol, &
      & rhs=rhs, &
      & alu=obj%pmat%A, &
      & jlu=obj%pmat%JA, &
      & ju=obj%pmat%JU)
  ELSE
    CALL LUSOLVE( &
      & sol=sol, &
      & rhs=rhs, &
      & alu=obj%pmat%A, &
      & jlu=obj%pmat%JA, &
      & ju=obj%pmat%JU)
  END IF

END IF
END PROCEDURE mField_ILUSOLVE1

!----------------------------------------------------------------------------
!                                                                   LUSOLVE
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_ILUSOLVE2
REAL(DFP), POINTER :: solval(:)
REAL(DFP), POINTER :: rhsval(:)
solval => sol%getPointer()
rhsval => rhs%getPointer()
CALL obj%ILUSOLVE(sol=solval, rhs=rhsval, isTranspose=isTranspose)
NULLIFY (solval, rhsval)
END PROCEDURE mField_ILUSOLVE2

END SUBMODULE LUSolveMethods

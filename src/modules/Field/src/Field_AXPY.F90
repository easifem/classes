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

MODULE Field_AXPY
USE GlobalData, ONLY: stderr, DFP, I4B, LGT
USE BaseMethod, ONLY: AXPY, SCAL
USE AbstractField_Class
USE AbstractNodeField_Class
USE AbstractMatrixField_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "Field_AXPY"

INTERFACE AXPY
  MODULE PROCEDURE AXPY1, AXPY2
END INTERFACE AXPY

PUBLIC :: AXPY

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-31
! summary: v1 = a1*v1 + a2 * v2

SUBROUTINE AXPY1(a1, v1, a2, v2)
  REAL(DFP), INTENT(IN) :: a1
  CLASS(AbstractNodeField_), INTENT(INOUT) :: v1
  REAL(DFP), INTENT(IN) :: a2
  CLASS(AbstractNodeField_), INTENT(IN) :: v2
  !
  !
  !
  CHARACTER(*), PARAMETER :: myName = "AXPY1"
  !
  IF (.NOT. v1%isInitiated) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'AbstractNodeField_::v1 is not initiated')
  END IF
  !
  IF (.NOT. v2%isInitiated) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'AbstractNodeField_::v2 is not initiated')
  END IF
  !
  CALL SCAL(X=v1%realVec, A=a1)
  CALL AXPY(Y=v1%realVec, A=a2, X=v2%realVec)
  !
END SUBROUTINE AXPY1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-31
! summary: v1 = a1*v1 + a2 * matvec(m2, v2)

SUBROUTINE AXPY2(a1, v1, a2, m2, v2, isTranspose2)
  REAL(DFP), INTENT(IN) :: a1
  CLASS(AbstractNodeField_), INTENT(INOUT) :: v1
  REAL(DFP), INTENT(IN) :: a2
  CLASS(AbstractMatrixField_), INTENT(IN) :: m2
  CLASS(AbstractNodeField_), INTENT(IN) :: v2
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose2
  !
  CHARACTER(*), PARAMETER :: myName = "AXPY2"
  !
  IF ( &
    &      .NOT. v1%isInitiated &
    & .OR. .NOT. v2%isInitiated &
    & .OR. .NOT. m2%isInitiated) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'AbstractNodeField_::v1, v2 or AbstractMatrixField_:: m2 &
      & is not initiated')
  END IF
  !
  ! v1=v1 * a1
  CALL SCAL(X=v1%realVec, A=a1)
  ! v1=v1+a2*Matvec(m2, v2)
  !
  CALL m2%MatVec(x=v2, y=v1, isTranspose=isTranspose2, &
    & addContribution=.TRUE., scale=a2)
  !
END SUBROUTINE AXPY2

END MODULE Field_AXPY

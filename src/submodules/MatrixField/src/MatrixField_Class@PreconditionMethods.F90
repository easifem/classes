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

SUBMODULE(MatrixField_Class) PreconditionMethods
USE GlobalData, ONLY: PRECOND_ILUT
USE GlobalData, ONLY: PRECOND_ILUTP
USE GlobalData, ONLY: PRECOND_ILUD
USE GlobalData, ONLY: PRECOND_ILUDP
USE GlobalData, ONLY: PRECOND_ILUK
USE CSRMatrix_Method, ONLY: GetILUT
USE CSRMatrix_Method, ONLY: GetILUTP
USE CSRMatrix_Method, ONLY: GetILUD
USE CSRMatrix_Method, ONLY: GetILUDP
USE CSRMatrix_Method, ONLY: GetILUK
USE CSRMatrix_Method, ONLY: CSRMatrix_Size => Size
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "MatrixField_Class@PreconditionMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                      reversePermutation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_reversePermutation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_reversePermutation"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_reversePermutation

!----------------------------------------------------------------------------
!                                                           GetPrecondition
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrecondition
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPrecondition"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPrecondition

!----------------------------------------------------------------------------
!                                                         ApplyDBCtoPrecond
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-03
! summary: Thsi algo does not work, I am working onit

SUBROUTINE ApplyDBCtoPrecond(obj, dbcPtrs)
  CLASS(MatrixField_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: dbcPtrs(:)

  INTEGER(I4B) :: i, ii
  LOGICAL(LGT), ALLOCATABLE :: mask(:)

  ASSOCIATE (JA => obj%Pmat%JA, A => obj%Pmat%A, nrow => obj%Pmat%nrow)

    ALLOCATE (mask(nrow))
    mask = .FALSE.
    mask(dbcPtrs) = .TRUE.

    DO CONCURRENT(i=1:SIZE(dbcPtrs))
      ii = dbcPtrs(i)
      A(JA(ii):JA(ii + 1) - 1) = 0.0_DFP
    END DO

    DO CONCURRENT(i=1:nrow)
      DO ii = JA(i), JA(i + 1) - 1
        IF (mask(JA(ii))) THEN
          A(ii) = 0.0_DFP
        END IF
      END DO
    END DO

    A(dbcPtrs) = 1.0_DFP

    DEALLOCATE (mask)

  END ASSOCIATE
END SUBROUTINE ApplyDBCtoPrecond

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE PreconditionMethods

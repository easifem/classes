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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains matrix vector method for [[MatrixField_]]

SUBMODULE(MatrixFieldLis_Class) MatVecMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Matvec
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Matvec2
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_Matvec2"
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: isTranspose0
LOGICAL(LGT) :: addContribution0
REAL(DFP) :: scale0
INTEGER(INT64) :: temp_lis_ptr

CALL lis_vector_is_null(x%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. x%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either AbstractNodeField_::x is not initiated'// &
  & " or, x%lis_ptr is not available")
END IF

CALL lis_vector_is_null(y%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. y%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either AbstractNodeField_::y is not initiated'// &
  & " or, y%lis_ptr is not available")
END IF

isTranspose0 = input(option=isTranspose, default=.FALSE.)
addContribution0 = input(option=addContribution, default=.FALSE.)
scale0 = input(option=scale, default=1.0_DFP)

IF (addContribution0) THEN
  CALL lis_vector_duplicate(y%lis_ptr, temp_lis_ptr, ierr)
  CALL CHKERR(ierr)
  IF (isTranspose0) THEN
    CALL lis_matvech(obj%lis_ptr, x%lis_ptr, temp_lis_ptr, ierr)
    CALL CHKERR(ierr)
  ELSE
    CALL lis_matvec(obj%lis_ptr, x%lis_ptr, temp_lis_ptr, ierr)
    CALL CHKERR(ierr)
  END IF
  CALL lis_vector_axpy(scale0, temp_lis_ptr, y%lis_ptr, ierr)
  CALL CHKERR(ierr)
  CALL lis_vector_destroy(temp_lis_ptr, ierr)
  CALL CHKERR(ierr)
ELSE
  IF (isTranspose0) THEN
    CALL lis_matvech(obj%lis_ptr, x%lis_ptr, y%lis_ptr, ierr)
    CALL CHKERR(ierr)
  ELSE
    CALL lis_matvec(obj%lis_ptr, x%lis_ptr, y%lis_ptr, ierr)
    CALL CHKERR(ierr)
  END IF
END IF

END PROCEDURE obj_Matvec2

END SUBMODULE MatVecMethods

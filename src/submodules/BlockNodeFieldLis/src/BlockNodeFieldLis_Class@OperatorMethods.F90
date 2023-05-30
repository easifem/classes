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

SUBMODULE(BlockNodeFieldLis_Class) OperatorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_isEqual
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "bnField_isEqual"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize2
REAL(DFP) :: value1
REAL(DFP) :: value2

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (.NOT. obj2%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeField_::obj2 is not initiated'// &
  & "")
END IF

tsize = obj%SIZE()
tsize2 = obj2%SIZE()

IF (tsize .NE. tsize2) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'size of obj and obj2 are not same')
END IF

DO ii = 1, tsize
  CALL obj%getSingle(VALUE=value1, indx=ii)
  CALL obj2%getSingle(VALUE=value2, indx=ii)
  IF (.NOT. (value1.approxeq.value2)) THEN
    ans = .FALSE.
    RETURN
  END IF
END DO

END PROCEDURE bnField_isEqual

END SUBMODULE OperatorMethods

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

SUBMODULE(VectorFieldLis_Class) BlasMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Norm2
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "vField_Norm2"
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  CALL lis_vector_nrm2(obj%lis_ptr, ans, ierr)
  CALL CHKERR(ierr)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'VectorFieldLis_::obj is NOT AVAILABLE')
END IF
END PROCEDURE vField_Norm2

!----------------------------------------------------------------------------
!                                                                      Norm1
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Norm1
CHARACTER(*), PARAMETER :: myName = "vField_Norm1"
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  CALL lis_vector_nrm1(obj%lis_ptr, ans, ierr)
  CALL CHKERR(ierr)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'VectorFieldLis_::obj is NOT AVAILABLE')
END IF
END PROCEDURE vField_Norm1

!----------------------------------------------------------------------------
!                                                                      Normi
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_Normi
CHARACTER(*), PARAMETER :: myName = "vField_Normi"
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  CALL lis_vector_nrmi(obj%lis_ptr, ans, ierr)
  CALL CHKERR(ierr)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'VectorFieldLis_::obj is NOT AVAILABLE')
END IF
END PROCEDURE vField_Normi

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE BlasMethods

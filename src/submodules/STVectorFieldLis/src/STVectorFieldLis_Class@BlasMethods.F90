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

SUBMODULE(STVectorFieldLis_Class) BlasMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm2
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_Norm2"
INTEGER(I4B) :: ierr
CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  CALL lis_vector_nrm2(obj%lis_ptr, ans, ierr)
  CALL CHKERR(ierr)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'STVectorFieldLis_::obj is NOT AVAILABLE')
END IF
END PROCEDURE obj_Norm2

!----------------------------------------------------------------------------
!                                                                      Norm1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm1
CHARACTER(*), PARAMETER :: myName = "obj_Norm1"
INTEGER(I4B) :: ierr
CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  CALL lis_vector_nrm1(obj%lis_ptr, ans, ierr)
  CALL CHKERR(ierr)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'STVectorFieldLis_::obj is NOT AVAILABLE')
END IF
END PROCEDURE obj_Norm1

!----------------------------------------------------------------------------
!                                                                      Normi
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Normi
CHARACTER(*), PARAMETER :: myName = "obj_Normi"
INTEGER(I4B) :: ierr
CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  CALL lis_vector_nrmi(obj%lis_ptr, ans, ierr)
  CALL CHKERR(ierr)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'STVectorFieldLis_::obj is NOT AVAILABLE')
END IF
END PROCEDURE obj_Normi

!----------------------------------------------------------------------------
!                                                                      Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

SELECT TYPE (obj2)
CLASS is (STVectorFieldLis_)
  CALL obj%Set(VALUE=obj2)
CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for obj2 type')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE BlasMethods

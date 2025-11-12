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

SUBMODULE(VectorFieldLis_Class) IOMethods
USE Display_Method, ONLY: Display
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer, &
                                   AbstractNodeFieldDisplay

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (ierr .NE. LIS_FALSE) THEN
  CALL Display(msg, unitno=unitno)
  CALL Display("obj%lis_ptr not available.", unitno=unitno)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(obj)
CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
CALL CHKERR(ierr)

NULLIFY (realvec)
CALL AbstractNodeFieldDisplay(obj=obj, msg=msg, unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods

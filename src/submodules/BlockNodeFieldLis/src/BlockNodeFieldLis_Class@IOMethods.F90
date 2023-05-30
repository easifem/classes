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

SUBMODULE(BlockNodeFieldLis_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Display
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "bnField_Display"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  realvec => AbstractNodeFieldGetPointer(obj)
  CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
  CALL CHKERR(ierr)
  NULLIFY (realvec)
  CALL AbstractNodeFieldDisplay(obj=obj, msg=msg, unitno=unitno)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'BlockNodeFieldLis_::obj is NOT AVAILABLE')
END IF

END PROCEDURE bnField_Display

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Import
CHARACTER(*), PARAMETER :: myName = "bnField_Import"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine is under construction!')
END PROCEDURE bnField_Import

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_Export
CHARACTER(*), PARAMETER :: myName = "bnField_Export"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START] Export()')

CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  realvec => AbstractNodeFieldGetPointer(obj)
  CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
  CALL CHKERR(ierr)
  NULLIFY (realvec)
  CALL AbstractNodeFieldExport(obj=obj, hdf5=hdf5, group=group)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & 'BlockNodeFieldLis_::obj%lis_ptr is NOT AVAILABLE')
END IF

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Export()")
END PROCEDURE bnField_Export

END SUBMODULE IOMethods

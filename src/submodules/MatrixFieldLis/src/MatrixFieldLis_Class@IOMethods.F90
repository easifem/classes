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

SUBMODULE(MatrixFieldLis_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Display
CALL MatrixFieldDisplay(obj=obj, msg=msg, unitno=unitno)
IF (obj%engine%chars() .EQ. "LIS_OMP") THEN
  IF (ALLOCATED(obj%lis_ia)) THEN
    CALL Display("# lis_ia ALLOCATED", unitNo=unitNo)
  ELSE
    CALL Display("# lis_ia NOT ALLOCATED", unitNo=unitNo)
  END IF
  IF (ALLOCATED(obj%lis_ja)) THEN
    CALL Display("# lis_ja ALLOCATED", unitNo=unitNo)
  ELSE
    CALL Display("# lis_ja NOT ALLOCATED", unitNo=unitNo)
  END IF
END IF
END PROCEDURE mField_Display

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Export
CHARACTER(*), PARAMETER :: myName = "mField_Export"
TYPE(String) :: dname

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[START] Export()')

CALL MatrixFieldExport(obj=obj, hdf5=hdf5, group=group)
IF (ALLOCATED(obj%lis_ia)) THEN
  dname = TRIM(group)//"/lis_ia"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%lis_ia)
END IF
IF (ALLOCATED(obj%lis_ja)) THEN
  dname = TRIM(group)//"/lis_ja"
  CALL hdf5%WRITE(dsetname=TRIM(dname%chars()), &
  & vals=obj%lis_ja)
END IF

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[END] Export()')

END PROCEDURE mField_Export

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Import
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "mField_Import"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: nnz

CALL MatrixFieldImport(  &
  & obj=obj,  &
  & hdf5=hdf5,  &
  & group=group,  &
  & dom=dom, &
  & domains=domains)

CALL lis_matrix_create(obj%comm, obj%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_matrix_set_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

nnz = getNNZ(obj%mat)
obj%lis_ia = obj%mat%csr%ia - 1
obj%lis_ja = obj%mat%csr%ja - 1

CALL lis_matrix_set_csr( &
  & nnz, &
  & obj%lis_ia, &
  & obj%lis_ja, &
  & obj%mat%a, &
  & obj%lis_ptr, &
  & ierr)
CALL CHKERR(ierr)

CALL lis_matrix_assemble(obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_get_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)
CALL CHKERR(ierr)
END PROCEDURE mField_Import

END SUBMODULE IOMethods

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
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(BlockMatrixFieldLis_Class) HDFMethods
USE String_Class, ONLY: String
USE MatrixField_Class, ONLY: MatrixFieldImport, &
                             MatrixFieldExport
USE CSRMatrix_Method, ONLY: GetNNZ

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

TYPE(String) :: dname
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL MatrixFieldExport(obj=obj, hdf5=hdf5, group=group)

isok = ALLOCATED(obj%lis_ia)
IF (isok) THEN
  dname = TRIM(group)//"/lis_ia"
  CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%lis_ia)
END IF

isok = ALLOCATED(obj%lis_ja)
IF (isok) THEN
  dname = TRIM(group)//"/lis_ja"
  CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%lis_ja)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

INTEGER(I4B) :: ierr, nnz

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL MatrixFieldImport( &
  obj=obj, hdf5=hdf5, group=group, fedof=fedof, fedofs=fedofs)

CALL lis_matrix_create(obj%comm, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_matrix_set_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

nnz = GetNNZ(obj%mat)
obj%lis_ia = obj%mat%csr%ia - 1
obj%lis_ja = obj%mat%csr%ja - 1

CALL lis_matrix_set_csr(nnz, obj%lis_ia, obj%lis_ja, obj%mat%a, obj%lis_ptr, &
                        ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_matrix_assemble(obj%lis_ptr, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_matrix_get_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_matrix_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods

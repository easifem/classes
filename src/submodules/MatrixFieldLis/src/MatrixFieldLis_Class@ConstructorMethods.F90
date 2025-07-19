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
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixFieldLis_Class) ConstructorMethods

USE MatrixField_Class, ONLY: MatrixFieldInitiate, &
                             MatrixFieldDeallocate

USE CSRMatrix_Method, ONLY: GetNNZ
IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: nnz

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL MatrixFieldInitiate(obj=obj, param=param, fedof=fedof)

CALL lis_matrix_create(obj%comm, obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_set_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

nnz = GetNNZ(obj%mat)

obj%lis_ia = obj%mat%csr%ia - 1
obj%lis_ja = obj%mat%csr%ja - 1

CALL lis_matrix_set_csr(nnz, obj%lis_ia, obj%lis_ja, obj%mat%a, obj%lis_ptr, &
                        ierr)

CALL CHKERR(ierr)

CALL lis_matrix_assemble(obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_get_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)
CALL CHKERR(ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: nnz

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL MatrixFieldInitiate(obj=obj, obj2=obj2, copyFull=copyFull, &
                          copyStructure=copyStructure, usePointer=usePointer)

CALL lis_matrix_create(obj%comm, obj%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_matrix_set_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

nnz = getNNZ(obj%mat)
obj%lis_ia = obj%mat%csr%ia - 1
obj%lis_ja = obj%mat%csr%ja - 1

CALL lis_matrix_set_csr(nnz, obj%lis_ia, obj%lis_ja, obj%mat%a, obj%lis_ptr, &
                        ierr)
CALL CHKERR(ierr)

CALL lis_matrix_assemble(obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_get_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)
CALL CHKERR(ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
INTEGER(I4B), PARAMETER :: tVar = 2
INTEGER(I4B) :: ierr
INTEGER(I4B) :: nnz

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL MatrixFieldInitiate(obj=obj, param=param, fedof=fedof)
CALL lis_matrix_create(obj%comm, obj%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_matrix_set_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

nnz = getNNZ(obj%mat)
obj%lis_ia = obj%mat%csr%ia - 1
obj%lis_ja = obj%mat%csr%ja - 1

CALL lis_matrix_set_csr(nnz, obj%lis_ia, obj%lis_ja, obj%mat%a, obj%lis_ptr, &
                        ierr)
CALL CHKERR(ierr)

CALL lis_matrix_assemble(obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_get_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
CALL CHKERR(ierr)

CALL lis_matrix_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)
CALL CHKERR(ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ierr
CALL lis_matrix_unset(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_matrix_destroy(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (ALLOCATED(obj%lis_ia)) DEALLOCATE (obj%lis_ia)
IF (ALLOCATED(obj%lis_ja)) DEALLOCATE (obj%lis_ja)
CALL MatrixFieldDeallocate(obj)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                      Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

END SUBMODULE ConstructorMethods

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

SUBMODULE(MatrixFieldLis_Class) DBCMethods
USE ReallocateUtility, ONLY: Reallocate
USE CSRMatrix_Method, ONLY: CSRMatrix_GetSubMatrix => GetSubMatrix
USE CSRMatrix_Method, ONLY: CSRMatrix_Matvec => Matvec
USE CSRMatrix_Method, ONLY: CSRMatrix_ApplyDBC => ApplyDBC
USE CSRMatrix_Method, ONLY: CSRMatrix_GetNNZ => GetNNZ
USE Display_Method, ONLY: ToString
USE MatrixField_Class, ONLY: MatrixFieldApplyDirichletBC
USE BaseType, ONLY: math => TypeMathOpt
USE InputUtility, ONLY: Input

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                           ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC1()"
#endif

LOGICAL(LGT) :: isok, isDBCPtrsPresent
INTEGER(I4B) :: ierr, submat_nnz

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL MatrixFieldApplyDirichletBC(obj=obj, dbcPtrs=dbcPtrs)

IF (.NOT. obj%isSubmatInit) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'no submatrix initialized, returning')

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isDBCPtrsPresent = PRESENT(dbcPtrs)
IF (.NOT. isDBCPtrsPresent) THEN

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
    'dbcPtrs not present, there is not need to initiate submatrix, returning')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

CALL lis_matrix_create(obj%comm, obj%submat_lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_matrix_set_size(obj%submat_lis_ptr, obj%local_n, &
                         obj%global_n, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

submat_nnz = CSRMatrix_GetNNZ(obj%submat)
obj%submat_lis_ia = obj%submat%csr%ia - 1
obj%submat_lis_ja = obj%submat%csr%ja - 1

CALL lis_matrix_set_csr( &
  submat_nnz, obj%submat_lis_ia, obj%submat_lis_ja, obj%submat%a, &
  obj%submat_lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_matrix_assemble(obj%submat_lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_matrix_get_range(obj%submat_lis_ptr, obj%submat_is, obj%submat_ie, &
                          ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC1

!----------------------------------------------------------------------------
!                                                             GetDBCSubMat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCSubMat
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCSubMat()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetDirichletBCSubMat

!----------------------------------------------------------------------------
!                                                       ApplyDBCToRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBCToRHS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBCToRHS()"
#endif

LOGICAL(LGT) :: addContribution0
INTEGER(I4B) :: ierr
INTEGER(INT64) :: temp_lis_ptr
REAL(DFP) :: scale0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

addContribution0 = Input(option=addContribution, default=math%no)
scale0 = Input(option=scale, default=math%one)

IF (addContribution0) THEN
  ! initiating temp_lis_ptr from y
  CALL lis_vector_duplicate(y%lis_ptr, temp_lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  ! computing matvec Ax into temp_lis_ptr
  CALL lis_matvec(obj%submat_lis_ptr, x%lis_ptr, temp_lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  ! y = y + scale * Ax
  CALL lis_vector_axpy(scale0, temp_lis_ptr, y%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  CALL lis_vector_destroy(temp_lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

CALL lis_matvec(obj%lis_ptr, x%lis_ptr, y%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBCToRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE DBCMethods

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

SUBMODULE(AbstractNodeField_Class) BlasMethods
IMPLICIT NONE

#ifdef USE_LIS
#include "lisf.h"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                     AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AXPY
CHARACTER(*), PARAMETER :: myName = "obj_AXPY()"
LOGICAL(LGT) :: problem
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)

#ifdef DEBUG_VER
  problem = x%engine .NE. "NATIVE_SERIAL"
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: engine of x should be NATIVE_SERIAL.')
    RETURN
  END IF
#endif

  CALL AXPY(X=x%realvec, Y=obj%realvec, A=scale)

#ifdef USE_LIS
CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHKERR(ierr)

  CALL lis_vector_is_null(x%lis_ptr, ierr)
  CALL CHKERR(ierr)
#endif

  ! alpha, x, y, ierr
  CALL lis_vector_axpy(scale, x%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_AXPY

!----------------------------------------------------------------------------
!                                                                     SCAL
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SCAL
CHARACTER(*), PARAMETER :: myName = "obj_SCAL()"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)

  CALL SCAL(x=obj%realvec, A=scale)

#ifdef USE_LIS
CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHKERR(ierr)
#endif

  ! alpha, x, ierr
  CALL lis_vector_scale(scale, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_SCAL

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_COPY
CHARACTER(*), PARAMETER :: myName = "obj_COPY()"
LOGICAL(LGT) :: problem
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)

#ifdef DEBUG_VER
  problem = obj2%engine .NE. "NATIVE_SERIAL"
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: engine of obj2 should be NATIVE_SERIAL.')
    RETURN
  END IF
#endif

  CALL COPY(y=obj%realvec, x=obj2%realvec)

#ifdef USE_LIS

CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHECKERR(ierr)

  CALL lis_vector_is_null(obj2%lis_ptr, ierr)
  CALL CHECKERR(ierr)
#endif

  CALL lis_vector_copy(obj2%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_COPY

!----------------------------------------------------------------------------
!                                                                    NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm2
CHARACTER(*), PARAMETER :: myName = "obj_Norm2()"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

ans = 0.0_DFP

SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)
  ans = NORM2(obj=obj%realvec)

#ifdef USE_LIS

CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHKERR(ierr)
#endif

  CALL lis_vector_nrm2(obj%lis_ptr, ans, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Norm2

!----------------------------------------------------------------------------
!                                                                    NORM1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm1
CHARACTER(*), PARAMETER :: myName = "obj_Norm1()"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

ans = 0.0

SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)
  ans = NORM1(obj=obj%realvec)

#ifdef USE_LIS

CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHKERR(ierr)
#endif

  CALL lis_vector_nrm1(obj%lis_ptr, ans, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Norm1

!----------------------------------------------------------------------------
!                                                                    NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Normi
CHARACTER(*), PARAMETER :: myName = "obj_Normi()"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

ans = 0.0_DFP
SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)
  ans = NORMi(obj=obj%realvec)

#ifdef USE_LIS

CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHKERR(ierr)
#endif

  CALL lis_vector_nrmi(obj%lis_ptr, ans, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Normi

!----------------------------------------------------------------------------
!                                                               DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DOT_PRODUCT
CHARACTER(*), PARAMETER :: myName = "obj_DOT_PRODUCT()"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

ans = 0.0_DFP

SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)
  ans = DOT_PRODUCT(obj1=obj%realvec, obj2=obj2%realvec)

#ifdef USE_LIS
CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHKERR(ierr)

  CALL lis_vector_is_null(obj2%lis_ptr, ierr)
  CALL CHKERR(ierr)
#endif

  CALL lis_vector_dot(obj%lis_ptr, obj2%lis_ptr, ans, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                               DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PMUL
CHARACTER(*), PARAMETER :: myName = "obj_PMUL()"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)
  CALL PMUL(obj=obj%realvec, obj1=obj1%realvec, obj2=obj2%realvec)

#ifdef USE_LIS
CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHKERR(ierr)
  CALL lis_vector_is_null(obj1%lis_ptr, ierr)
  CALL CHKERR(ierr)
  CALL lis_vector_is_null(obj2%lis_ptr, ierr)
  CALL CHKERR(ierr)
#endif

  CALL lis_vector_pmul(obj1%lis_ptr, obj2%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_PMUL

!----------------------------------------------------------------------------
!                                                                 Reciprocal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Reciprocal
CHARACTER(*), PARAMETER :: myName = "obj_Reciprocal()"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

SELECT CASE (obj%engine%chars())
CASE (TypeEngineName%native_serial)
  CALL Reciprocal(obj1=obj%realvec, obj2=obj%realvec)

#ifdef USE_LIS

CASE (TypeEngineName%lis_omp)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj%lis_ptr, ierr)
  CALL CHKERR(ierr)
#endif

  CALL lis_vector_reciprocal(obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#endif

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found given engine = '//  &
    & obj%engine%chars())
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Reciprocal

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE BlasMethods

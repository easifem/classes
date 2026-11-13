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

SUBMODULE(ScalarFieldLis_Class) HDFMethods
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldExport
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldImport
USE EngineOpt_Class, ONLY: TypeEngineOpt
IMPLICIT NONE

#include "lisf.h"

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "ScalarFieldLis_Class@HDFMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL lis_vector_is_null(obj%lis_ptr, ierr)

#ifdef DEBUG_VER
isok = ierr .EQ. LIS_FALSE
CALL AssertError1(isok, myName, &
                  "ScalarFieldLis_ is NOT available")
#endif

realvec => AbstractNodeFieldGetPointer(obj)
CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

NULLIFY (realvec)
CALL AbstractNodeFieldExport(obj=obj, hdf5=hdf5, group=group)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Destroy the LIS vector of obj, if any
CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN
  CALL lis_vector_destroy(obj%lis_ptr, ierr)
#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif
END IF

! Read the node field data: sizes, DOF data and realVec
CALL AbstractNodeFieldImport(obj=obj, hdf5=hdf5, group=group, &
                             fedof=fedof, fedofs=fedofs, &
                             timefedof=timefedof, timefedofs=timefedofs, &
                             geofedof=geofedof, geofedofs=geofedofs)

! The file may come from a native field
obj%engine = TypeEngineOpt%lis_omp

! Create the LIS vector and copy realVec into it
CALL lis_vector_create(obj%comm, obj%lis_ptr, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_vector_set_size(obj%lis_ptr, obj%local_n, obj%global_n, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

CALL lis_vector_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

realvec => AbstractNodeFieldGetPointer(obj)
CALL lis_vector_scatter(realvec, obj%lis_ptr, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

NULLIFY (realvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods

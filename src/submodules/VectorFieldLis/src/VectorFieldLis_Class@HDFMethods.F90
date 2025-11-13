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

SUBMODULE(VectorFieldLis_Class) HDFMethods
USE String_Class, ONLY: String
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer, &
                                   AbstractNodeFieldExport, &
                                   AbstractNodeFieldImport

USE VectorField_Class, ONLY: VectorFieldExport

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

TYPE(String) :: dsetname
LOGICAL(LGT) :: bools(3), isok
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractNodeFieldImport( &
  obj=obj, hdf5=hdf5, group=group, fedof=fedof, fedofs=fedofs, &
  geofedof=geofedof, geofedofs=geofedofs)

! spaceCompo
dsetname = TRIM(group)//"/spaceCompo"

#ifdef DEBUG_VER
isok = hdf5%pathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset spaceCompo should be present')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%spaceCompo)

dsetname = TRIM(group)//"/tSize"
bools(1) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/dof"
bools(2) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/realVec"
bools(3) = hdf5%pathExists(dsetname%chars())

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! isok = ALL(bools)
!
! IF (.NOT. isok) THEN
!
!   CALL param%initiate()
!
!   CALL SetVectorFieldParam( &
!     param=param, name=obj%name%chars(), fieldType=obj%fieldType, &
!     spaceCompo=obj%spaceCompo, engine=obj%engine%chars())
!
!   obj%isInit = .FALSE.
!   CALL obj%Initiate(param=param, fedof=fedof, geofedof=geofedof)
!   CALL param%DEALLOCATE()
!
!   CALL FinishMe
!   RETURN
!
! END IF

! till now we have read tSize, dof, and realVec
! but we have not created lis vector
! so we create lis_vector
! then we update values from realvec to lis_vector using scatter methods

CALL lis_vector_create(obj%comm, obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_vector_set_size(obj%lis_ptr, obj%local_n, &
                         obj%global_n, ierr)

CALL CHKERR(ierr)

CALL lis_vector_get_range(obj%lis_ptr, obj%is, obj%ie, ierr)
CALL CHKERR(ierr)

realvec => AbstractNodeFieldGetPointer(obj)
CALL lis_vector_scatter(realvec, obj%lis_ptr, ierr)

CALL CHKERR(ierr)

NULLIFY (realvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                  Export
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
                  "obj%lis_ptr is not found")
#endif

realvec => AbstractNodeFieldGetPointer(obj)
CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
CALL CHKERR(ierr)
NULLIFY (realvec)
CALL VectorFieldExport(obj=obj, hdf5=hdf5, group=group)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods

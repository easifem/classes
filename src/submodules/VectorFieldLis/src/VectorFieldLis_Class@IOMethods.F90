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
USE String_Class, ONLY: String
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer, &
                                   AbstractNodeFieldDisplay, &
                                   AbstractNodeFieldExport, &
                                   AbstractNodeFieldImport

USE VectorField_Class, ONLY: VectorFieldDisplay, &
                             VectorFieldExport, &
                             SetVectorFieldParam

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (ierr .NE. LIS_FALSE) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'VectorFieldLis_::obj is NOT AVAILABLE')
  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(obj)
CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
CALL CHKERR(ierr)

NULLIFY (realvec)
CALL AbstractNodeFieldDisplay(obj=obj, msg=msg, unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: dsetname
TYPE(ParameterList_) :: param
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
isok = hdf5%pathExists(dsetname%chars())
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: The dataset spaceCompo should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%spaceCompo)

dsetname = TRIM(group)//"/tSize"
bools(1) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/dof"
bools(2) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/realVec"
bools(3) = hdf5%pathExists(dsetname%chars())

isok = ALL(bools)

IF (.NOT. isok) THEN

  CALL param%initiate()

  CALL SetVectorFieldParam( &
    param=param, name=obj%name%chars(), fieldType=obj%fieldType, &
    spaceCompo=obj%spaceCompo, engine=obj%engine%chars())

  obj%isInitiated = .FALSE.
  CALL obj%Initiate(param=param, fedof=fedof, geofedof=geofedof)
  CALL param%DEALLOCATE()

  CALL FinishMe
  RETURN

END IF

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

CALL FinishMe

CONTAINS
SUBROUTINE finishMe

  CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                          "[END]")
END SUBROUTINE finishMe

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                  Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (ierr .EQ. LIS_FALSE) THEN
  realvec => AbstractNodeFieldGetPointer(obj)
  CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
  CALL CHKERR(ierr)
  NULLIFY (realvec)
  CALL VectorFieldExport(obj=obj, hdf5=hdf5, group=group)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods

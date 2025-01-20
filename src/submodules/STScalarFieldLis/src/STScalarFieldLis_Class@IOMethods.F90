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

SUBMODULE(STScalarFieldLis_Class) IOMethods
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_Display"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (ierr .EQ. LIS_FALSE) THEN

  realvec => AbstractNodeFieldGetPointer(obj)
  CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
  CALL CHKERR(ierr)
  NULLIFY (realvec)
  CALL STScalarFieldDisplay(obj=obj, msg=msg, unitno=unitno)

ELSE

  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'STScalarFieldLis_::obj is NOT AVAILABLE')

END IF

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                 Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_Export"
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
  CALL STScalarFieldExport(obj=obj, hdf5=hdf5, group=group)
ELSE
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & 'STScalarFieldLis_::obj%lis_ptr is NOT AVAILABLE')
END IF

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Export()")
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                                 Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import"
TYPE(String) :: dsetname
LOGICAL(LGT) :: bools(3)
TYPE(ParameterList_) :: param
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ierr

! info
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[START] Import()")

CALL AbstractNodeFieldImport( &
  & obj=obj, &
  & hdf5=hdf5, &
  & group=group, &
  & dom=dom, &
  & domains=domains)

! timeCompo
dsetname = TRIM(group)//"/timeCompo"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%timeCompo)
ELSE
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset timeCompo should be present')
END IF

dsetname = TRIM(group)//"/tSize"
bools(1) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/dof"
bools(2) = hdf5%pathExists(dsetname%chars())
dsetname = TRIM(group)//"/realVec"
bools(3) = hdf5%pathExists(dsetname%chars())

IF (.NOT. ALL(bools)) THEN

  CALL param%initiate()
  CALL setSTScalarFieldParam( &
    & param=param, &
    & name=obj%name%chars(), &
    & fieldType=obj%fieldType, &
    & timeCompo=obj%timeCompo, &
    & engine=obj%engine%chars() &
    & )
  obj%isInitiated = .FALSE.
  CALL obj%initiate(param=param, dom=dom)
  CALL param%DEALLOCATE()

ELSE

  ! till now we have read tSize, dof, and realVec
  ! but we have not created lis vector
  ! so we create lis_vector
  ! then we update values from realvec to lis_vector using scatter methods

  CALL lis_vector_create(obj%comm, obj%lis_ptr, ierr)
  CALL CHKERR(ierr)

  CALL lis_vector_set_size(obj%lis_ptr, obj%local_n, &
  & obj%global_n, ierr)
  CALL CHKERR(ierr)

  CALL lis_vector_get_range( &
  & obj%lis_ptr, &
  & obj%is, &
  & obj%ie, &
  & ierr &
  & )
  CALL CHKERR(ierr)

  realvec => AbstractNodeFieldGetPointer(obj)
  CALL lis_vector_scatter(realvec, obj%lis_ptr, ierr)
  CALL CHKERR(ierr)
  NULLIFY (realvec)

END IF

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "[END] Import()")

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods

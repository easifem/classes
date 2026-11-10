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

SUBMODULE(AbstractField_Class) HDFMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractField_Class@HDFMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Export
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

IF (.NOT. obj%IsInitiated()) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! Check
#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, &
                  'hdf5 file is not opened')
#endif

! Check
#ifdef DEBUG_VER
isok = hdf5%isWrite()
CALL AssertError1(isok, myName, &
                  'hdf5 file does not have write permission')
#endif

! Bool0
! ---------
! isInit
dname = TRIM(group)//"/Bool0/isInit"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%isInit)

! IntR0
! ---------
! fieldType
dname = TRIM(group)//"/IntR0/fieldType"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%fieldType)

! comm
dname = TRIM(group)//"/IntR0/comm"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%comm)

! myRank
dname = TRIM(group)//"/IntR0/myRank"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%myRank)

! numProcs
dname = TRIM(group)//"/IntR0/numProcs"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%numProcs)

! global_n
dname = TRIM(group)//"/IntR0/global_n"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%global_n)

! local_n
dname = TRIM(group)//"/IntR0/local_n"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%local_n)

! is
dname = TRIM(group)//"/IntR0/is"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%is)

! ie
dname = TRIM(group)//"/IntR0/ie"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%ie)

! lis_ptr
dname = TRIM(group)//"/IntR0/lis_ptr"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=math%zero_i)

! String0
! engine
dname = TRIM(group)//"/StringR0/name"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%name)

! engine
dname = TRIM(group)//"/StringR0/engine"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%engine)

! The node data (dof, FEDOF pointers and boundary conditions) is exported
! by the child classes, see AbstractNodeField_

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                             obj_Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
LOGICAL(LGT) :: isok
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Check
#ifdef DEBUG_VER
isok = hdf5%isOpen()
CALL AssertError1(isok, myName, 'HDF5 file is not opened')
#endif

! Check
#ifdef DEBUG_VER
isok = hdf5%isRead()
CALL AssertError1(isok, myName, 'HDF5 file does not have read permission')
#endif

obj%isInit = math%yes

! Bool0

! IntR0

! fieldType
dsetname = TRIM(group)//"/IntR0/fieldType"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%fieldType)
END IF

! comm
dsetname = TRIM(group)//"/IntR0/comm"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%comm)
END IF

! myRank
dsetname = TRIM(group)//"/IntR0/myRank"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%myRank)
END IF

! numProcs
dsetname = TRIM(group)//"/IntR0/numProcs"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%numProcs)
END IF

! global_n
dsetname = TRIM(group)//"/IntR0/global_n"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%global_n)
END IF

! local_n
dsetname = TRIM(group)//"/IntR0/local_n"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%local_n)
END IF

! is
dsetname = TRIM(group)//"/IntR0/is"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%is)
END IF

! ie
dsetname = TRIM(group)//"/IntR0/ie"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%ie)
END IF

! lis_ptr
! dsetname = TRIM(group)//"/IntR0/lis_ptr"
! isok = hdf5%pathExists(dsetname%chars())
! IF (isok) THEN
!   CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%lis_ptr)
! END IF

! StringR0
! --------

! name
dsetname = TRIM(group)//"/StringR0/name"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)
END IF

! engine
dsetname = TRIM(group)//"/StringR0/engine"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%engine)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods

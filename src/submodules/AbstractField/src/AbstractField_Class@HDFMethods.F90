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
USE Display_Method, ONLY: Display, ToString
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
LOGICAL(LGT) :: isok
#endif

TYPE(String) :: dname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%isInitiated
CALL AssertError1(isok, myName, &
                  'AbstractField_::obj%isInitiated is not initiated')
#endif

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

! fieldType
dname = TRIM(group)//"/fieldType"
CALL hdf5%WRITE(dsetname=dname%chars(), &
                vals=STRING(TypeField%ToString(obj%fieldType)))

! name
dname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%name)

! engine
dname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%engine)

! comm
dname = TRIM(group)//"/comm"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%comm)

! myRank
dname = TRIM(group)//"/myRank"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%myRank)

! numProcs
dname = TRIM(group)//"/numProcs"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%numProcs)

! local_n
dname = TRIM(group)//"/local_n"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%local_n)

! global_n
dname = TRIM(group)//"/global_n"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%global_n)

! is
dname = TRIM(group)//"/is"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%is)

! ie
dname = TRIM(group)//"/ie"
CALL hdf5%WRITE(dsetname=dname%chars(), vals=obj%ie)

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

TYPE(String) :: strval, dsetname
INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! main program
#ifdef DEBUG_VER
isok = .NOT. obj%isInitiated
CALL AssertError1(isok, myName, &
                  'The instance of AbstractField_ is already initiated')
#endif

! Check
#ifdef DEBUG_VER
isok = hdf5%isOpen()
CALL AssertError1(isok, myName, &
                  'HDF5 file is not opened')
#endif

! Check
#ifdef DEBUG_VER
isok = hdf5%isRead()
CALL AssertError1(isok, myName, &
                  'HDF5 file does not have read permission')
#endif

! fieldType
dsetname = TRIM(group)//"/fieldType"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
  obj%fieldType = TypeField%ToNumber(strval%chars())
ELSE
  obj%fieldType = TypeField%normal
END IF

! name
dsetname = TRIM(group)//"/name"

#ifdef DEBUG_VER
isok = hdf5%pathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset name should be present')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)

! engine
dsetname = TRIM(group)//"/engine"
#ifdef DEBUG_VER
isok = hdf5%pathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset engine should be present')
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%engine)

! comm
dsetname = TRIM(group)//"/comm"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%comm)
ELSE
  obj%comm = 0
END IF

! myRank
dsetname = TRIM(group)//"/myRank"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%myRank)
ELSE
  obj%myRank = 0
END IF

! numProcs
dsetname = TRIM(group)//"/numProcs"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%numProcs)
ELSE
  obj%numProcs = 1
END IF

! global_n
dsetname = TRIM(group)//"/global_n"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%global_n)
ELSE
  obj%global_n = 1
END IF

! local_n
dsetname = TRIM(group)//"/local_n"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%local_n)
ELSE
  obj%local_n = 1
END IF

! is
dsetname = TRIM(group)//"/is"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%is)
ELSE
  obj%is = 1
END IF

! ie
dsetname = TRIM(group)//"/ie"
isok = hdf5%pathExists(dsetname%chars())
IF (isok) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%ie)
ELSE
  obj%ie = 1
END IF

#ifdef DEBUG_VER
isok = .NOT. ASSOCIATED(obj%fedof)
CALL AssertError1(isok, myName, &
                  'AbstractField_::obj%fedof is already associated')
#endif

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(obj%fedofs)
CALL AssertError1(isok, myName, &
                  'AbstractField_::obj%fedofs is already allocated')
#endif

obj%isInitiated = .TRUE.

isok = PRESENT(fedof)
IF (isok) THEN
  obj%fedof => fedof

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(geofedof)
IF (isok) THEN
  obj%geofedof => geofedof

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(fedofs)
IF (isok) THEN
  tsize = SIZE(fedofs)
  ALLOCATE (obj%fedofs(tsize))

  DO ii = 1, tsize
    obj%fedofs(ii)%ptr => fedofs(ii)%ptr
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(geofedofs)
IF (isok) THEN
  tsize = SIZE(geofedofs)
  ALLOCATE (obj%geofedofs(tsize))

  DO ii = 1, tsize
    obj%geofedofs(ii)%ptr => geofedofs(ii)%ptr
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(.FALSE., myName, &
                  "For non-rectangle matrix dom should be present, "// &
                  "for rectangle matrix matrix fedofs should be present")
#endif

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

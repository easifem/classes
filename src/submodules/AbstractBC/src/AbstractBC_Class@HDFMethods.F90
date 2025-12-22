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

SUBMODULE(AbstractBC_Class) HDFMethods
USE Display_Method, ONLY: ToString, Display
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
LOGICAL(LGT) :: isok
#endif

TYPE(String) :: dsetname, strval
REAL(DFP) :: real0
REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        "[START]")
#endif

CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
isok = .NOT. obj%isInit
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj is already initiated, deallocate first')
#endif

obj%isInit = .TRUE.
obj%dom => dom

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, &
                  'HDF5 file is not opened, open it first')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsRead()
CALL AssertError1(isok, myName, &
                  'HDF5 file does not have read permission')
#endif

dsetname = TRIM(group)//"/name"

#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset name should be present.')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)

dsetname = TRIM(group)//"/idof"

#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset idof should be present.')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%idof)

dsetname = TRIM(group)//"/nodalValueType"

#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset nodalValueType should be present.')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
obj%nodalValueType = TypeFieldOpt%ToNumber(strval%chars())

dsetname = TRIM(group)//"/isUserFunction"

#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset isUserFunction should be present.')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isUserFunction)

IF (obj%isUserFunction) THEN
  ALLOCATE (obj%func)
  dsetname = TRIM(group)//"/userFunction"
  CALL obj%func%IMPORT(hdf5=hdf5, group=dsetname%chars())
END IF

dsetname = TRIM(group)//"/isUseExternal"

#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset isUseExternal should be present.')
#endif

CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isUseExternal)

dsetname = TRIM(group)//"/boundary"

#ifdef DEBUG_VER
isok = hdf5%PathExists(dsetname%chars())
CALL AssertError1(isok, myName, &
                  'The dataset Boundary, which is a group, should be present')
#endif

CALL obj%boundary%IMPORT(hdf5=hdf5, group=dsetname%chars())

IF (.NOT. obj%isUserFunction) THEN
  dsetname = TRIM(group)//"/nodalValue"

#ifdef DEBUG_VER
  isok = hdf5%pathExists(dsetname%chars())
  CALL AssertError1(isok, myName, &
                    'The dataset nodalValue should be present')
#endif

  SELECT CASE (obj%nodalValueType)

  CASE (fevaropt%constant)
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=real0)
    CALL Reallocate(obj%nodalValue, 1, 1)
    obj%nodalValue = real0

  CASE (fevaropt%space, fevaropt%time)
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=real1)
    CALL Reallocate(obj%nodalValue, SIZE(real1), 1)
    obj%nodalValue(:, 1) = real1

  CASE (fevaropt%spaceTime)
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=real2)
    obj%nodalValue = real2

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
          'No case found for nodalValueType = '//ToString(obj%nodalValueType))
#endif

  END SELECT
END IF

dsetname = ''
strval = ''
IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

TYPE(String) :: dsetname, strval
REAL(DFP) :: real0
REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, 'AbstractBC_::obj is not initiated, &
  &initiate it first')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, 'HDF5 file is not opened')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsWrite()
CALL AssertError1(isok, myName, 'HDF5 file does not have write permission')
#endif

! write name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

! write idof
dsetname = TRIM(group)//"/idof"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%idof)

! write nodalValueType
dsetname = TRIM(group)//"/nodalValueType"
strval = TypeFieldOpt%ToString(obj%nodalValueType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

! write isUserFunction
dsetname = TRIM(group)//"/isUserFunction"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isUserFunction)

isok = ASSOCIATED(obj%func)
dsetname = TRIM(group)//"/userFunction"
IF (isok) CALL obj%func%Export(hdf5=hdf5, group=dsetname%chars())

! write isUseExternal
dsetname = TRIM(group)//"/isUseExternal"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isUseExternal)

! write boundary
dsetname = TRIM(group)//"/boundary"
CALL obj%boundary%Export(hdf5=hdf5, group=dsetname%chars())

! Read nodalValue
IF (.NOT. obj%isUserFunction) THEN

  dsetname = TRIM(group)//"/nodalValue"

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myName, &
                'nodalValue is not allocated, it seems nodalValue is not set')
#endif

  SELECT CASE (obj%nodalValueType)

  CASE (fevaropt%constant)
    real0 = obj%nodalValue(1, 1)
    CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=real0)

  CASE (fevaropt%space, fevaropt%time)
    real1 = obj%nodalValue(:, 1)
    CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=real1)

  CASE (fevaropt%spaceTime)
    real2 = obj%nodalValue
    CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=real2)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
          'No case found for nodalValueType = '//ToString(obj%nodalValueType))
#endif

  END SELECT

END IF

IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)
dsetname = ''; strval = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods

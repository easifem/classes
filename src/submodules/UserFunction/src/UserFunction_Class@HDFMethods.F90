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

SUBMODULE(UserFunction_Class) HDFMethods
USE Display_Method, ONLY: Display
USE BaseType, ONLY: varopt => TypeFEVariableOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "UserFunction_Class@HDFMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

TYPE(String) :: dsetname, strval
REAL(DFP), ALLOCATABLE :: vectorValue(:), matrixValue(:, :)
INTEGER(I4B) :: tsize, nrow, ncol
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        "[START]")
#endif

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, &
                  "HDF file is not opened.")
#endif

#ifdef DEBUG_VER
isok = hdf5%IsRead()
CALL AssertError1(isok, myName, &
                  "HDF file does not have read permission.")
#endif

! name
dsetname = TRIM(group)//"/name"
#ifdef DEBUG_VER
isok = hdf5%pathExists(dsetname%Chars())
CALL AssertError1(isok, myName, &
                  dsetname//" is not present.")
#endif
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)

! isUserFunctionSet
dsetname = TRIM(group)//"/isExternalFunc"
isok = hdf5%pathExists(dsetname%Chars())
obj%isExternalFunc = math%no
IF (isok) &
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isExternalFunc)

#ifdef DEBUG_VER
isok = .NOT. obj%isExternalFunc
CALL AssertError1(isok, myName, &
                  'Currently, import does not work for external func')
#endif

!> returnType
dsetname = TRIM(group)//"/returnType"
#ifdef DEBUG_VER
isok = hdf5%pathExists(dsetname%Chars())
CALL AssertError1(isok, myName, &
                  dsetname//" is not present.")
#endif
CALL hdf5%READ(dsetname=dsetname%Chars(), vals=strval)
obj%returnType = UserFunctionGetReturnType(strval%Chars())

!> argType
dsetname = TRIM(group)//"/argType"
#ifdef DEBUG_VER
isok = hdf5%pathExists(dsetname%Chars())
CALL AssertError1(isok, myName, &
                  dsetname//" is not present.")
#endif
CALL hdf5%READ(dsetname=dsetname%Chars(), vals=strval)
obj%argType = UserFunctionGetArgType(strval%Chars())

!> check the argType, and decide the importer
#ifdef DEBUG_VER
CALL AssertError2(obj%argType, varopt%constant, myName, &
                  "a%obj%argType, b=varopt%constant")
#endif

!> scalarValue, vectorValue, matrixValue
SELECT CASE (obj%returnType)

CASE (varopt%scalar)
  dsetname = TRIM(group)//"/scalarValue"
  isok = hdf5%pathExists(dsetname%chars())
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%scalarValue)
  END IF

CASE (varopt%vector)
  dsetname = TRIM(group)//"/vectorValue"
  isok = hdf5%pathExists(dsetname%chars())
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=vectorValue)
    tsize = SIZE(vectorValue)
    obj%vectorValue(1:tsize) = vectorValue(1:tsize)
    DEALLOCATE (vectorValue)
  END IF

CASE (varopt%matrix)
  dsetname = TRIM(group)//"/matrixValue"
  isok = hdf5%pathExists(dsetname%chars())
  IF (isok) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=matrixValue)
    nrow = SIZE(matrixValue, 1)
    ncol = SIZE(matrixValue, 2)
    obj%matrixValue(1:nrow, 1:ncol) = matrixValue(1:nrow, 1:ncol)
    DEALLOCATE (matrixValue)
  END IF

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "no case found for varopt%rank...")
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

TYPE(String) :: dsetname, strval
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, &
                  "HDF file is not opened.")
#endif

#ifdef DEBUG_VER
isok = hdf5%IsWrite()
CALL AssertError1(isok, myName, &
                  "HDF file does not have write permission.")
#endif

dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

#ifdef DEBUG_VER
isok = .NOT. obj%isExternalFunc
CALL AssertError1(isok, myName, &
                  'Currently, export function does not work '// &
                  ' for UserFunction.')
#endif

! the following code works when obj%isExternalFunc is not true
dsetname = TRIM(group)//"/isExternalFunc"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isExternalFunc)

!> returnType
dsetname = TRIM(group)//"/returnType"
strval = NAME_RETURN_TYPE(obj%returnType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

!> argType
dsetname = TRIM(group)//"/argType"
strval = NAME_ARG_TYPE(obj%argType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
!>

#ifdef DEBUG_VER
CALL AssertError3(obj%argType, varopt%constant, myName, &
                  "a=obj%argType, b=varopt%constant")
#endif

! the following code is executed when obj%argType .eq. varopt%constant
SELECT CASE (obj%returnType)

CASE (varopt%Scalar)
  dsetname = TRIM(group)//"/scalarValue"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%scalarValue)

CASE (varopt%vector)
  dsetname = TRIM(group)//"/vectorValue"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                  vals=obj%vectorValue(1:obj%numReturns))

CASE (varopt%matrix)
  dsetname = TRIM(group)//"/matrixValue"
  CALL hdf5%WRITE( &
    dsetname=dsetname%chars(), &
    vals=obj%matrixValue(1:obj%returnShape(1), 1:obj%returnShape(2)))

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "no case found for obj%returnType")
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods

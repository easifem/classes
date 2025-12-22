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

SUBMODULE(UserFunction_Class) IOMethods
USE Display_Method, ONLY: Display
USE BaseType, ONLY: varopt => TypeFEVariableOpt
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: bool1

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isInit, "isInit: ", unitNo=unitNo)

IF (.NOT. obj%isInit) RETURN

CALL Display("name: "//obj%name, unitNo=unitNo)
CALL Display(obj%isUserFunctionSet, "isUserFunctionSet: ", unitNo=unitNo)
CALL Display(obj%isLuaScript, "isLuaScript: ", unitNo=unitNo)
IF (obj%isLuaScript) THEN
  CALL Display(obj%luaScript%chars(), "luaScript: ", unitNo=unitNo)
  CALL Display(obj%luaFunctionName%chars(), "luaFunctionName: ", &
               unitNo=unitNo)
END IF

CALL Display(NAME_RETURN_TYPE(obj%returnType), "returnType: ", &
             unitNo=unitNo)
IF (obj%returnType .EQ. varopt%matrix) THEN
  CALL Display(obj%returnShape, "shape of returnType: ", unitNo=unitNo)
END IF

CALL Display(NAME_ARG_TYPE(obj%argType), "argType: ", &
             unitNo=unitNo)

CALL Display(obj%numArgs, "number of arguments: ", unitNo=unitNo)
CALL Display(obj%numReturns, "number of returns: ", unitNo=unitNo)

bool1 = ASSOCIATED(obj%scalarFunction)
CALL Display(bool1, msg="scalarFunction ASSOCIATED: ", unitno=unitno)

bool1 = ASSOCIATED(obj%vectorFunction)
CALL Display(bool1, msg="vectorFunction ASSOCIATED: ", unitno=unitno)

bool1 = ASSOCIATED(obj%matrixFunction)
CALL Display(bool1, msg="matrixFunction ASSOCIATED: ", unitno=unitno)

IF (obj%argType .EQ. varopt%constant) THEN
  SELECT CASE (obj%returnType)
  CASE (varopt%Scalar)
    CALL Display(obj%scalarValue, "scalarValue: ", unitNo=unitNo)
  CASE (varopt%Vector)
    IF (ALLOCATED(obj%vectorValue)) THEN
      CALL Display(obj%vectorValue, "vectorValue: ", unitNo=unitNo)
    ELSE
      CALL Display("vectorValue: NOT ALLOCATED", unitNo=unitNo)
    END IF
  CASE (varopt%Matrix)
    IF (ALLOCATED(obj%matrixValue)) THEN
      CALL Display(obj%matrixValue, "matrixValue: ", unitNo=unitNo)
    ELSE
      CALL Display("matrixValue: NOT ALLOCATED", unitNo=unitNo)
    END IF
  END SELECT
END IF

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
                        "[START]")
#endif

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                '[INTERNAL ERROR] :: HDF5 file does not have read permission')
END IF

!> name
dsetname = TRIM(group)//"/name"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[CONFIG ERROR] :: name should be present.')
END IF

!> isUserFunctionSet
dsetname = TRIM(group)//"/isUserFunctionSet"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%isUserFunctionSet)
ELSE
  obj%isUserFunctionSet = .FALSE.
END IF

!> isUserFunctionSet
IF (obj%isUserFunctionSet) THEN
  ! dsetname = TRIM(group)//"/userFunction"
  ! ALLOCATE (obj%userFunction)
  ! CALL obj%userFunction%IMPORT(hdf5=hdf5, group=dsetname%chars())
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[WIP ERROR] :: currently import does not work for useFunction')
END IF

IF (.NOT. obj%isUserFunctionSet) THEN
  !> returnType
  dsetname = TRIM(group)//"/returnType"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                  'dsetname '//dsetname%chars()//'is not present in HDFFile_')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    obj%returnType = UserFunctionGetReturnType(strval%chars())
  END IF

  !> argType
  dsetname = TRIM(group)//"/argType"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                  'dsetname '//dsetname%chars()//'is not present in HDFFile_')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    obj%argType = UserFunctionGetArgType(strval%chars())
  END IF

  !> check the argType, and decide the importer
  IF (obj%argType .EQ. varopt%constant) THEN
    !> scalarValue, vectorValue, matrixValue
    SELECT CASE (obj%returnType)
    CASE (varopt%scalar)
      !> scalarValue
      dsetname = TRIM(group)//"/scalarValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%scalarValue)
      END IF
    CASE (varopt%vector)
      !> vectorValue
      dsetname = TRIM(group)//"/vectorValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%vectorValue)
      END IF
    CASE (varopt%matrix)
      !> matrixValue
      dsetname = TRIM(group)//"/matrixValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%matrixValue)
      END IF
    END SELECT
  ELSE
    CALL e%RaiseError(modName//'::'//myName//" - "// &
               'Currently, EASIFEM Supports import of constant userFunction.')
  END IF
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export"
TYPE(String) :: dsetname, strval

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] Export()')
#endif

!> check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

!> check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: HDF5 file does not have write permission')
END IF

!> name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

!> isUserFunctionSet
IF (obj%isUserFunctionSet) THEN
  !> isUserFunctionSet
  ! dsetname = TRIM(group)//"/isUserFunctionSet"
  ! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isUserFunctionSet)
  ! !> returnType
  ! dsetname = TRIM(group)//"/userFunction"
  ! CALL obj%userFunction%Export(hdf5=hdf5, group=dsetname%chars())
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                 '[WIP ERROR] :: Currently export function does not work '// &
                    ' for UserFunction.')
END IF

IF (.NOT. obj%isUserFunctionSet) THEN
  !> isUserFunctionSet
  dsetname = TRIM(group)//"/isUserFunctionSet"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isUserFunctionSet)

  !> returnType
  dsetname = TRIM(group)//"/returnType"
  strval = NAME_RETURN_TYPE(obj%returnType)
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

  !> argType
  dsetname = TRIM(group)//"/argType"
  strval = NAME_ARG_TYPE(obj%argType)
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)
  !>
  IF (obj%argType .EQ. varopt%constant) THEN
    SELECT CASE (obj%returnType)
    CASE (varopt%Scalar)
      !> scalarValue
      dsetname = TRIM(group)//"/scalarValue"
      CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%scalarValue)
    CASE (varopt%vector)
      !> vectorValue
      dsetname = TRIM(group)//"/vectorValue"
      IF (ALLOCATED(obj%vectorValue)) &
        CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%vectorValue)
    CASE (varopt%matrix)
      !> matrixValue
      dsetname = TRIM(group)//"/matrixValue"
      IF (ALLOCATED(obj%matrixValue)) &
        CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%matrixValue)
    END SELECT
  ELSE
    CALL e%RaiseError(modName//'::'//myName//" - "// &
               'Currently, EASIFEM Supports import of constant userFunction.')
  END IF
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods

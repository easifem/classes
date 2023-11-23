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
USE BaseMethod
USE TomlUtility
USE tomlf, ONLY:  &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_array,  &
  & toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Display
LOGICAL(LGT) :: bool1
CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isInitiated, "isInitiated: ", unitNo=unitNo)

IF (.NOT. obj%isInitiated) THEN
  RETURN
END IF

CALL Display(obj%isUserFunctionSet, "isUserFunctionSet: ", unitNo=unitNo)
CALL Display(obj%isLuaScript, "isLuaScript: ", unitNo=unitNo)
IF (obj%isLuaScript) THEN
  CALL display(obj%luaScript, "luaScript: ", unitNo=unitNo)
  CALL display(obj%luaFunctionName, "luaFunctionName: ", unitNo=unitNo)
END IF

CALL Display(NAME_RETURN_TYPE(obj%returnType), "returnType: ",  &
  & unitNo=unitNo)
IF (obj%returnType .EQ. Matrix) THEN
  CALL Display(obj%returnShape, "Shape of returnType: ", unitNo=unitNo)
END IF

CALL Display(NAME_ARG_TYPE(obj%argType), "argType: ",  &
  & unitNo=unitNo)

CALL Display(obj%numArgs, "number of arguments: ", unitNo=unitNo)
CALL Display(obj%numReturns, "number of returns: ", unitNo=unitNo)

bool1 = ASSOCIATED(obj%scalarFunction)
CALL Display(bool1, msg="scalarFunction ASSOCIATED: ", unitno=unitno)

bool1 = ASSOCIATED(obj%vectorFunction)
CALL Display(bool1, msg="vectorFunction ASSOCIATED: ", unitno=unitno)

bool1 = ASSOCIATED(obj%matrixFunction)
CALL Display(bool1, msg="matrixFunction ASSOCIATED: ", unitno=unitno)

IF (obj%argType .EQ. CONSTANT) THEN
  SELECT CASE (obj%returnType)
  CASE (Scalar)
    CALL Display(obj%scalarValue, "scalarValue: ", unitNo=unitNo)
  CASE (Vector)
    IF (ALLOCATED(obj%vectorValue)) THEN
      CALL Display(obj%vectorValue, "vectorValue: ", unitNo=unitNo)
    ELSE
      CALL Display("vectorValue: NOT ALLOCATED", unitNo=unitNo)
    END IF
  CASE (Matrix)
    IF (ALLOCATED(obj%matrixValue)) THEN
      CALL Display(obj%matrixValue, "matrixValue: ", unitNo=unitNo)
    ELSE
      CALL Display("matrixValue: NOT ALLOCATED", unitNo=unitNo)
    END IF
  END SELECT
END IF

END PROCEDURE auf_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Import
CHARACTER(*), PARAMETER :: myName = "auf_Import"
TYPE(String) :: dsetname, strval
!> main
!> info
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "Importing the instance of UserFunction_ data")
!> check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF
!> check
IF (.NOT. hdf5%isRead()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
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
    & '[WIP ERROR] :: currently import does not work for useFunction')
ELSE
  !> returnType
  dsetname = TRIM(group)//"/returnType"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'dsetname '//dsetname%chars()//'is not present in HDFFile_')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    obj%returnType = UserFunctionGetReturnType(strval%chars())
  END IF
  !> argType
  dsetname = TRIM(group)//"/argType"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'dsetname '//dsetname%chars()//'is not present in HDFFile_')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
    obj%argType = UserFunctionGetArgType(strval%chars())
  END IF
  !> check the argType, and decide the importer
  IF (obj%argType .EQ. CONSTANT) THEN
    !> scalarValue, vectorValue, matrixValue
    SELECT CASE (obj%returnType)
    CASE (SCALAR)
      !> scalarValue
      dsetname = TRIM(group)//"/scalarValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%scalarValue)
      END IF
    CASE (VECTOR)
      !> vectorValue
      dsetname = TRIM(group)//"/vectorValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%vectorValue)
      END IF
    CASE (MATRIX)
      !> matrixValue
      dsetname = TRIM(group)//"/matrixValue"
      IF (hdf5%pathExists(dsetname%chars())) THEN
        CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%matrixValue)
      END IF
    END SELECT
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Currently, EASIFEM Supports import of constant userFunction.')
  END IF
END IF
END PROCEDURE auf_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Export
CHARACTER(*), PARAMETER :: myName = "auf_Export"
TYPE(String) :: dsetname, strval

!> info
CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "Exporting the instance of UserFunction_ data")

!> check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

!> check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have write permission')
END IF

!> isUserFunctionSet
IF (obj%isUserFunctionSet) THEN
  !> isUserFunctionSet
  ! dsetname = TRIM(group)//"/isUserFunctionSet"
  ! CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isUserFunctionSet)
  ! !> returnType
  ! dsetname = TRIM(group)//"/userFunction"
  ! CALL obj%userFunction%Export(hdf5=hdf5, group=dsetname%chars())
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: Currently export function does not work '//  &
    & ' for UserFunction.')
ELSE
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
  IF (obj%argType .EQ. CONSTANT) THEN
    SELECT CASE (obj%returnType)
    CASE (SCALAR)
      !> scalarValue
      dsetname = TRIM(group)//"/scalarValue"
      CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%scalarValue)
    CASE (VECTOR)
      !> vectorValue
      dsetname = TRIM(group)//"/vectorValue"
      IF (ALLOCATED(obj%vectorValue)) &
        & CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%vectorValue)
    CASE (MATRIX)
      !> matrixValue
      dsetname = TRIM(group)//"/matrixValue"
      IF (ALLOCATED(obj%matrixValue)) &
        & CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%matrixValue)
    END SELECT
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'Currently, EASIFEM Supports import of constant userFunction.')
  END IF
END IF
END PROCEDURE auf_Export

!----------------------------------------------------------------------------
!                                                        ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_ImportParamFromToml
CHARACTER(*), PARAMETER :: myName = "auf_ImportParamFromToml()"
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: bool1, isLuaScript, isFound
INTEGER(I4B) :: argType, returnType, numReturns, numArgs
TYPE(String) :: astr, luaScript, luaFunctionName
INTEGER(I4B), ALLOCATABLE :: returnShape(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportParamFromToml()')
#endif

CALL toml_get(table, "returnType", astr%raw, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: returnType should be present, and '//  &
    & 'it is a string.')
  RETURN
END IF
returnType = UserFunctionGetReturnType(astr%chars())

CALL Reallocate(returnShape, 2)
returnShape = [0, 0]
IF (returnType .EQ. Matrix) THEN
  CALL GetValue(table=table, key="returnShape", VALUE=returnShape,  &
    & origin=origin, stat=stat, isFound=isFound)
  bool1 = (stat .NE. toml_stat%success) .OR. (.NOT. isFound)
  IF (bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: when returnType is Matrix, then '//  &
      & 'returnShape should be present. It is a vector of integers'//  &
      & ' and size 2.')
    RETURN
  END IF
END IF

CALL toml_get(table, "argType", astr%raw, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: argType should be present, and '//  &
    & 'it is a string.')
  RETURN
END IF
argType = UserFunctionGetArgType(astr%chars())

CALL toml_get(table, "numArgs", numArgs, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: numArgs should be present, and '//  &
    & 'it is a scalar integer number.')
  RETURN
END IF

CALL toml_get(table, "numReturns", numReturns, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] ::  numReturns should be present, and '//  &
    & 'it is a scalar integer number.')
  RETURN
END IF

CALL toml_get(table, "luaScript", luaScript%raw, origin=origin, stat=stat)
isLuaScript = stat .EQ. toml_stat%success

IF (isLuaScript) THEN
  CALL toml_get(table, "luaFunctionName", luaFunctionName%raw,  &
    & origin=origin, stat=stat)
  bool1 = stat .NE. toml_stat%success
  IF (bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] ::  when you specify luaScript you should also '//  &
      & 'specify luaFunctionName.')
    RETURN
  END IF
END IF

IF (isLuaScript) THEN
  CALL SetUserFunctionParam(param=param,  &
    & returnType=returnType,  &
    & returnShape=returnShape, &
    & argType=argType,  &
    & numArgs=numArgs,  &
    & numReturns=numReturns, &
    & luaScript=luaScript%chars(),  &
    & luaFunctionName=luaFunctionName%chars())
ELSE
  CALL SetUserFunctionParam(param=param,  &
    & returnType=returnType,  &
    & returnShape=returnShape, &
    & argType=argType,  &
    & numArgs=numArgs,  &
    & numReturns=numReturns)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportParamFromToml()')
#endif
END PROCEDURE auf_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                        ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "auf_ImportFromToml1()"
TYPE(ParameterList_) :: param
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: bool1, isFound
REAL(DFP) :: scalarValue
REAL(DFP), ALLOCATABLE :: vectorValue(:), matrixValue(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportFromToml()')
#endif

CALL param%Initiate()
CALL obj%ImportParamFromToml(param=param, table=table)
CALL obj%Initiate(param=param)

IF (.NOT. obj%isLuaScript) THEN
  SELECT CASE (obj%returnType)
  CASE (Scalar)
    CALL toml_get(table, "scalarValue", scalarValue,  &
    & origin=origin, stat=stat)
    IF (stat .NE. toml_stat%success) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: returnType is Scalar, and '//  &
        & CHAR_LF//'argType is Constant. '//  &
        & CHAR_LF//'Therefore, scalarValue should be present.')
      RETURN
    END IF
    CALL obj%Set(scalarValue=scalarValue)
  CASE (Vector)
    CALL GetValue(table=table, key="vectorValue",  &
      & VALUE=vectorValue, origin=origin, stat=stat, isFound=isFound)

    bool1 = (.NOT. isFound) .OR. (stat .NE. toml_stat%success)
    IF (bool1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: returnType is Vector, and '//  &
        & CHAR_LF//'argType is Constant. '//  &
        & CHAR_LF//'Therefore, vectorValue should be present.')
      RETURN
    END IF

    CALL obj%Set(vectorValue=vectorValue)
    IF (ALLOCATED(vectorValue)) DEALLOCATE (vectorValue)

  CASE (Matrix)
    CALL GetValue(table=table, key="matrixValue",  &
      & VALUE=matrixValue, origin=origin, stat=stat, isFound=isFound)

    bool1 = (.NOT. isFound) .OR. (stat .NE. toml_stat%success)
    IF (bool1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: returnType is Matrix, and '//  &
        & CHAR_LF//'argType is Constant. '//  &
        & CHAR_LF//'Therefore, matrixValue should be present.')
      RETURN
    END IF

    CALL obj%Set(matrixValue=matrixValue)
    IF (ALLOCATED(matrixValue)) DEALLOCATE (matrixValue)
  END SELECT
END IF

CALL param%DEALLOCATE()
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportFromToml()')
#endif
END PROCEDURE auf_ImportFromToml1

!----------------------------------------------------------------------------
!                                                        ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "auf_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportFromToml()')
#endif

IF (PRESENT(afile)) THEN
  CALL GetValue(table=table, afile=afile)
ELSEIF (PRESENT(filename)) THEN
  CALL GetValue(table=table, filename=filename)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: either filename or afile should be present!')
  RETURN
END IF

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE.,  &
  & stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: following error occured while reading '//  &
    & 'the toml file :: cannot find ['//tomlName//"] table in config.")
END IF

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF,  &
    & unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportParamFromToml()')
#endif
END PROCEDURE auf_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods

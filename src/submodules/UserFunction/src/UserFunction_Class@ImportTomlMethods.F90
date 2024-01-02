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

SUBMODULE(UserFunction_Class) ImportTomlMethods
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
!                                                        ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportParamFromToml
CHARACTER(*), PARAMETER :: myName = "obj_ImportParamFromToml()"
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: bool1, isLuaScript, isFound
INTEGER(I4B) :: argType, returnType, numReturns, numArgs
TYPE(String) :: astr, luaScript, luaFunctionName, name
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

astr = ""
CALL toml_get(table, "name", astr%raw, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] ::  name should be present, and '//  &
    & 'it is a string.')
  RETURN
END IF
name = astr%chars()

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
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'If numArgs should not present, '//  &
    & 'the default values are used')

  SELECT CASE (argType)
  CASE (Constant)
    numArgs = DEFAULT_NUM_ARG_CONSTANT
  CASE (Space)
    numArgs = DEFAULT_NUM_ARG_SPACE
  CASE (Time)
    numArgs = DEFAULT_NUM_ARG_TIME
  CASE (SpaceTime)
    numArgs = DEFAULT_NUM_ARG_SPACETIME
  CASE default
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[Config Error]:: No case found for argType')
    RETURN
  END SELECT
END IF

CALL toml_get(table, "numReturns", numReturns, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'numReturns should is not present, and '//  &
    & 'using default value for numReturns.')

  SELECT CASE (returnType)
  CASE (Scalar)
    numReturns = DEFAULT_NUM_ARG_SCALAR
  CASE (Vector)
    numReturns = DEFAULT_NUM_ARG_VECTOR
  CASE (Matrix)
    numReturns = DEFAULT_NUM_ARG_MATRIX
  CASE default
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: no case found for returnType, and '//  &
      & 'numReturns should be present, and '//  &
      & 'it is a scalar integer number.')
    RETURN
  END SELECT
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
  CALL SetUserFunctionParam(param=param,   &
    & name=name%chars(), &
    & returnType=returnType,  &
    & returnShape=returnShape, &
    & argType=argType,  &
    & numArgs=numArgs,  &
    & numReturns=numReturns, &
    & luaScript=luaScript%chars(),  &
    & luaFunctionName=luaFunctionName%chars())
ELSE
  CALL SetUserFunctionParam(param=param,   &
    & name=name%chars(), &
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
END PROCEDURE obj_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                        ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
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
    CALL toml_get(table, "value", scalarValue,  &
    & origin=origin, stat=stat)
    IF (stat .NE. toml_stat%success) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: returnType is Scalar, and '//  &
        & CHAR_LF//'argType is Constant. '//  &
        & CHAR_LF//'Therefore, value should be present.')
      RETURN
    END IF
    CALL obj%Set(scalarValue=scalarValue)

  CASE (Vector)
    CALL GetValue(table=table, key="value",  &
      & VALUE=vectorValue, origin=origin, stat=stat, isFound=isFound)

    bool1 = (.NOT. isFound) .OR. (stat .NE. toml_stat%success)
    IF (bool1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: returnType is Vector, and '//  &
        & CHAR_LF//'argType is Constant. '//  &
        & CHAR_LF//'Therefore, value should be present.')
      RETURN
    END IF

    CALL obj%Set(vectorValue=vectorValue)
    IF (ALLOCATED(vectorValue)) DEALLOCATE (vectorValue)

  CASE (Matrix)
    CALL GetValue(table=table, key="value",  &
      & VALUE=matrixValue, origin=origin, stat=stat, isFound=isFound)

    bool1 = (.NOT. isFound) .OR. (stat .NE. toml_stat%success)
    IF (bool1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: returnType is Matrix, and '//  &
        & CHAR_LF//'argType is Constant. '//  &
        & CHAR_LF//'Therefore, value should be present.')
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
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                        ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

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

NULLIFY (node)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ImportTomlMethods

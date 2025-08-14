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

SUBMODULE(UserFunction_Class) TomlMethods
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: CHAR_LF, stdout
USE Display_Method, ONLY: Display, ToString

USE TomlUtility, ONLY: GetValue, GetValue_

USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_array, &
                 toml_stat
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
                        '[START] ')
#endif

CALL toml_get(table, "returnType", astr%raw, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[CONFIG ERROR] :: returnType should be present, and '// &
                    'it is a string.')
  RETURN
END IF
returnType = UserFunctionGetReturnType(astr%chars())

astr = ""
CALL toml_get(table, "name", astr%raw, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[CONFIG ERROR] ::  name should be present, and '// &
                    'it is a string.')
  RETURN
END IF
name = astr%chars()

ALLOCATE (returnShape(2))
returnShape = [0, 0]

IF (returnType .EQ. varopt%Matrix) THEN
  CALL GetValue(table=table, key="returnShape", VALUE=returnShape, &
                origin=origin, stat=stat, isFound=isFound)
  bool1 = (stat .NE. toml_stat%success) .OR. (.NOT. isFound)
  IF (bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: when returnType is Matrix, then '// &
               'returnShape should be present. It is a vector of integers'// &
                      ' and size 2.')
    RETURN
  END IF
END IF

CALL toml_get(table, "argType", astr%raw, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[CONFIG ERROR] :: argType should be present, and '// &
                    'it is a string.')
  RETURN
END IF
argType = UserFunctionGetArgType(astr%chars())

CALL toml_get(table, "numArgs", numArgs, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  numArgs = GetDefaultNumArgs(argType)
END IF

CALL toml_get(table, "numReturns", numReturns, origin=origin, stat=stat)
bool1 = stat .NE. toml_stat%success
IF (bool1) THEN
  numReturns = GetDefaultNumReturns(returnType)
END IF

CALL toml_get(table, "luaScript", luaScript%raw, origin=origin, stat=stat)
isLuaScript = stat .EQ. toml_stat%success

IF (isLuaScript) THEN
  CALL toml_get(table, "luaFunctionName", luaFunctionName%raw, &
                origin=origin, stat=stat)
  bool1 = stat .NE. toml_stat%success
  IF (bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[CONFIG ERROR] ::  when you specify luaScript you should also '// &
                      'specify luaFunctionName.')
    RETURN
  END IF
END IF

IF (isLuaScript) THEN
  CALL SetUserFunctionParam(param=param, &
                            name=name%chars(), &
                            returnType=returnType, &
                            returnShape=returnShape, &
                            argType=argType, &
                            numArgs=numArgs, &
                            numReturns=numReturns, &
                            luaScript=luaScript%chars(), &
                            luaFunctionName=luaFunctionName%chars())
ELSE
  CALL SetUserFunctionParam(param=param, &
                            name=name%chars(), &
                            returnType=returnType, &
                            returnShape=returnShape, &
                            argType=argType, &
                            numArgs=numArgs, &
                            numReturns=numReturns)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ImportParamFromToml()')
#endif
END PROCEDURE obj_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                          ReadNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadNameFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadNameFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading name ...')
#endif

  CALL GetValue(table=table, key="name", VALUE=obj%name, &
                default_value=myprefix, origin=origin, &
                stat=stat, isFound=isok)

#ifdef DEBUG_VER
  IF (.NOT. isok) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
       'Cannot find/read "name" in the config file. Using default='//myprefix)
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadNameFromToml

!----------------------------------------------------------------------------
!                                                      ReadReturnTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadReturnTypeFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadReturnTypeFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading returnType ...')
#endif

  CALL GetValue(table=table, key="returnType", VALUE=astr, &
                default_value="NA", origin=origin, &
                stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'Cannot find/read "returnType" in the config file.')
#endif

  obj%returnType = UserFunctionGetReturnType(astr%chars())

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ReadReturnTypeFromToml

!----------------------------------------------------------------------------
!                                                     ReadReturnShapeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadReturnShapeFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadReturnShapeFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading returnShape ...')
#endif

  ! If returnType is not Matrix, then do nothing and return
  isok = obj%returnType .NE. varopt%matrix
  IF (isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                    'returnType is not Matrix, so returnShape is not needed.')
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  CALL GetValue_(table=table, key="returnShape", VALUE=obj%returnShape, &
                 origin=origin, stat=stat, isFound=isok, tsize=tsize)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
       'Cannot find/read "returnShape" in the config file. returnShape is &
       &needed when returnType is matrix. returnShape is a vector of integers&
       &of size 2')
#endif

#ifdef DEBUG_VER
  isok = tsize .EQ. 2
  CALL AssertError1(isok, myName, &
                  'returnShape should be a vector of integers of size 2. '// &
                    'However, it is of size '//ToString(tsize))
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadReturnShapeFromToml

!----------------------------------------------------------------------------
!                                                        ReadArgTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadArgTypeFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadArgTypeFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading argType ...')
#endif

  CALL GetValue(table=table, key="argType", VALUE=astr, &
                default_value="NA", origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'Cannot find/read "argType" in the config file.')
#endif

  obj%argType = UserFunctionGetArgType(astr%chars())

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadArgTypeFromToml

!----------------------------------------------------------------------------
!                                                       ReadNumArgsFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadNumArgsFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadNumArgsFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, default_value
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading numArgs ...')
#endif

  default_value = GetDefaultNumArgs(obj%argType)
  CALL GetValue(table=table, key="numArgs", VALUE=obj%numArgs, &
          default_value=default_value, origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadNumArgsFromToml

!----------------------------------------------------------------------------
!                                                      ReadNumReturnsFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadNumReturnsFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadNumReturnsFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, default_value
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading numReturns ...')
#endif

  default_value = GetDefaultNumReturns(obj%argType)
  CALL GetValue(table=table, key="numReturns", VALUE=obj%numReturns, &
          default_value=default_value, origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadNumReturnsFromToml

!----------------------------------------------------------------------------
!                                                      ReadLuaScriptFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadLuaScriptFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadLuaScriptFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading luaScript ...')
#endif

  CALL GetValue(table=table, key="luaScript", VALUE=obj%luaScript, &
                default_value="NA", origin=origin, stat=stat, &
                isFound=obj%isLuaScript)

  IF (.NOT. obj%isLuaScript) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  CALL GetValue(table=table, key="luaFunctionName", &
                VALUE=obj%luaFunctionName, &
                default_value="NA", origin=origin, stat=stat, &
                isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                 'Cannot find/read "luaFunctionName" in the config file. '// &
                    'luaFunctionName is needed when luaScript is present.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadLuaScriptFromToml

!----------------------------------------------------------------------------
!                                                     ReadScalarValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadScalarValueFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadScalarValueFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  REAL(DFP) :: areal
  REAL(DFP), PARAMETER :: default_value = 0.0_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (obj%isLuaScript) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            'isLuaScropt is true, so nothing to do here...')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  isok = obj%returnType .EQ. varopt%scalar
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                           'returnType is not scalar, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  CALL GetValue(table=table, key="value", VALUE=areal, &
          default_value=default_value, origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
      'returnType is Scalar and argType is Constant. Therefore, value &
      &should be present. Cannot find/read "value" in the config file. &
      &value is needed when returnType is scalar.')
#endif

  CALL obj%Set(scalarValue=areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading value ...')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ReadScalarValueFromToml

!----------------------------------------------------------------------------
!                                                    ReadVectorValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadVectorValueFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadVectorValueFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  REAL(DFP), ALLOCATABLE :: areal(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (obj%isLuaScript) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            'isLuaScropt is true, so nothing to do here...')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  isok = obj%returnType .EQ. varopt%Vector
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                           'returnType is not Vector, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading value ...')
#endif

  CALL GetValue(table=table, key="value", VALUE=areal, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
      'returnType is Vector and argType is Constant. Therefore, value &
      &should be present. Cannot find/read "value" in the config file. &
      &value is needed when returnType is Vector.')
#endif

  CALL obj%Set(VectorValue=areal)

  IF (ALLOCATED(areal)) DEALLOCATE (areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadVectorValueFromToml

!----------------------------------------------------------------------------
!                                                    ReadMatrixValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadMatrixValueFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadMatrixValueFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  REAL(DFP), ALLOCATABLE :: areal(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (obj%isLuaScript) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            'isLuaScropt is true, so nothing to do here...')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  isok = obj%returnType .EQ. varopt%matrix
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                           'returnType is not Matrix, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading value ...')
#endif

  CALL GetValue(table=table, key="value", VALUE=areal, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
      'returnType is Matrix and argType is Constant. Therefore, value &
      &should be present. Cannot find/read "value" in the config file. &
      &value is needed when returnType is Matrix.')
#endif

  CALL obj%Set(matrixValue=areal)

  IF (ALLOCATED(areal)) DEALLOCATE (areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadMatrixValueFromToml

!----------------------------------------------------------------------------
!                                                        ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ImportFromToml()')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL ReadNamefromToml(obj=obj, table=table)
CALL ReadReturnTypeFromToml(obj=obj, table=table)
CALL ReadReturnShapeFromToml(obj=obj, table=table)
CALL ReadArgTypeFromToml(obj=obj, table=table)
CALL ReadNumArgsFromToml(obj=obj, table=table)
CALL ReadNumReturnsFromToml(obj=obj, table=table)
CALL ReadLuaScriptFromToml(obj=obj, table=table)
CALL ReadScalarValueFromToml(obj=obj, table=table)
CALL ReadVectorValueFromToml(obj=obj, table=table)
CALL ReadMatrixValueFromToml(obj=obj, table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                        ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
LOGICAL(LGT) :: isok
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
        'Following error occured while reading toml file :: cannot find ['// &
                  tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

NULLIFY (node)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                         GetDefaultNumArgs
!----------------------------------------------------------------------------

FUNCTION GetDefaultNumArgs(argType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: argType
  INTEGER(I4B) :: ans

  SELECT CASE (argType)
  CASE (varopt%Constant)
    ans = DEFAULT_NUM_ARG_CONSTANT
  CASE (varopt%Space)
    ans = DEFAULT_NUM_ARG_SPACE
  CASE (varopt%Time)
    ans = DEFAULT_NUM_ARG_TIME
  CASE (varopt%SpaceTime)
    ans = DEFAULT_NUM_ARG_SPACETIME
  CASE DEFAULT
    ans = -1
  END SELECT

END FUNCTION GetDefaultNumArgs

!----------------------------------------------------------------------------
!                                                     GetDefaultNumReturns
!----------------------------------------------------------------------------

FUNCTION GetDefaultNumReturns(returnType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: returnType
  INTEGER(I4B) :: ans

  SELECT CASE (returnType)
  CASE (varopt%Scalar)
    ans = DEFAULT_NUM_ARG_SCALAR
  CASE (varopt%Vector)
    ans = DEFAULT_NUM_ARG_VECTOR
  CASE (varopt%Matrix)
    ans = DEFAULT_NUM_ARG_MATRIX
  CASE DEFAULT
    ans = -1
  END SELECT

END FUNCTION GetDefaultNumReturns

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

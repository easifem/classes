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
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE TomlUtility, ONLY: GetValue
USE TomlUtility, ONLY: GetValue_
USE tomlf, ONLY: toml_get => get_value

IMPLICIT NONE
CONTAINS

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
CALL ReadScalarEquationParserFromToml(obj=obj, table=table)
CALL ReadVectorEquationParserFromToml(obj=obj, table=table)
CALL ReadMatrixEquationParserFromToml(obj=obj, table=table)

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
CALL AssertError1( &
  isok, myName, &
  'Following error occured while reading toml file :: cannot find ['// &
  tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node)

NULLIFY (node)
DEALLOCATE (table)

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
    ans = funcopt%constFuncArgs
  CASE (varopt%Space)
    ans = funcopt%spaceFuncArgs
  CASE (varopt%Time)
    ans = funcopt%timeFuncArgs
  CASE (varopt%SpaceTime)
    ans = funcopt%spaceTimeFuncArgs
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
    ans = funcopt%scalarFuncNumReturns
  CASE (varopt%Vector)
    ans = funcopt%vectorFuncNumReturns
  CASE (varopt%Matrix)
    ans = funcopt%matrixFuncNumReturns
  CASE DEFAULT
    ans = -1
  END SELECT
END FUNCTION GetDefaultNumReturns

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

  CHARACTER(*), PARAMETER :: default_name = "UserFunction"
  TYPE(String) :: astr
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading name ...')
#endif

  CALL GetValue(table=table, key="name", VALUE=astr, &
                default_value=default_name, origin=origin, &
                stat=stat, isFound=isok)

  obj%name = astr%chars()

  astr = ""

#ifdef DEBUG_VER
  IF (.NOT. isok) THEN
    CALL e%RaiseDebug( &
      modName//'::'//myName//' - '// &
      'Cannot find/read "name" in the config file. Using default='// &
      default_name)
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
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading returnType ...')
#endif

  CALL GetValue( &
    table=table, key="returnType", VALUE=astr, default_value="NA", &
    origin=origin, stat=stat, isFound=isok)

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
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading returnShape ...')
#endif

  ! If returnType is not Matrix, then do nothing and return
  isok = obj%returnType /= varopt%matrix
  IF (isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
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
  isok = tsize == 2
  CALL AssertError1( &
    isok, myName, &
    'returnShape should be a vector of integers of size 2. However, &
    &it is of size '//ToString(tsize))
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
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
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
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading numArgs ...')
#endif

  default_value = GetDefaultNumArgs(obj%argType)
  CALL GetValue( &
    table=table, key="numArgs", VALUE=obj%numArgs, &
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
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading numReturns ...')
#endif

  default_value = GetDefaultNumReturns(obj%returnType)

  CALL GetValue( &
    table=table, key="numReturns", VALUE=obj%numReturns, &
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
  TYPE(String) :: luaScript, luaFunctionName

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading luaScript ...')
#endif

  CALL GetValue(table=table, key="luaScript", VALUE=luaScript, &
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
        VALUE=luaFunctionName, default_value="NA", origin=origin, stat=stat, &
                isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                 'Cannot find/read "luaFunctionName" in the config file. &
                 &luaFunctionName field is needed when luaScript is present. &
                  &May be you are using luaFunction instead of luaFunctionName.')
#endif

  CALL obj%SetLuaScript(luaScript=luaScript%chars(), &
                        luaFunctionName=luaFunctionName%chars())

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

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = obj%returnType == varopt%scalar
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'returnType is not scalar, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading value ...')
#endif

  CALL GetValue( &
    table=table, key="value", VALUE=areal, &
    default_value=math%zero, origin=origin, stat=stat, isFound=isok)

  IF (isok) CALL obj%SetScalarConstantVal(val=areal)

#ifdef DEBUG_VER
  IF (.NOT. isok) THEN
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'value not found in toml, so nothing to do here')
  END IF
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

  isok = obj%returnType == varopt%Vector
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'returnType is not Vector, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading value ...')
#endif

  CALL GetValue(table=table, key="value", VALUE=areal, &
                origin=origin, stat=stat, isFound=isok)

  IF (isok) THEN

#ifdef DEBUG_VER
    isok = ALLOCATED(areal)
    CALL AssertError1(isok, myName, &
                   "could not allocated areal while reading value from toml.")
#endif

    CALL obj%SetVectorConstantVal(val=areal)
  END IF

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
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'isLuaScropt is true, so nothing to do here...')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  isok = obj%returnType == varopt%matrix
  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'returnType is not Matrix, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
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

  CALL obj%SetMatrixConstantVal(val=areal)

  IF (ALLOCATED(areal)) DEALLOCATE (areal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadMatrixValueFromToml

!----------------------------------------------------------------------------
!                                                      ImportVarNamesFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportVarNamesFromToml(vars, table)
  CHARACTER(funcopt%maxVarLen), ALLOCATABLE, INTENT(INOUT) :: &
    vars(:)
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportVarNamesFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, tsize, ii
  LOGICAL(LGT) :: isok, isScalar
  CHARACTER(*), PARAMETER :: key = "vars"
  TYPE(String), ALLOCATABLE :: strs(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading vars from toml ...')
#endif

  CALL GetValue(table=table, key=key, VALUE=strs, stat=stat, origin=origin, &
                isFound=isok, isScalar=isScalar)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    "Cannot find key "//key//" in the toml table")
#endif

  IF (isScalar) THEN
    tsize = 1
  ELSE
    tsize = SIZE(strs)
  END IF

  ALLOCATE (vars(tsize))

  DO ii = 1, tsize
    vars(ii) = strs(ii)%Chars()
    strs(ii) = ""
  END DO

  DEALLOCATE (strs)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportVarNamesFromToml

!----------------------------------------------------------------------------
!                                                ImportScalarFuncStrFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportScalarFuncStrFromToml(funcStr, table)
  CHARACTER(*), INTENT(INOUT) :: funcStr
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportScalarFuncStrFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  TYPE(String) :: strs
  CHARACTER(*), PARAMETER :: key = "expression"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading expression from toml ...')
#endif

  CALL GetValue(table=table, key=key, VALUE=strs, stat=stat, origin=origin, &
                isFound=isok, default_value="NA")

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    "Cannot find key "//key//" in the toml table")
#endif

  funcStr = strs%Chars()
  strs = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportScalarFuncStrFromToml

!----------------------------------------------------------------------------
!                                                ImportVectorFuncStrFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportVectorFuncStrFromToml(funcStr, table)
  TYPE(String), ALLOCATABLE, INTENT(INOUT) :: funcStr(:)
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportVectorFuncStrFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok, isScalar
  CHARACTER(*), PARAMETER :: key = "expression"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading expression from toml ...')
#endif

  CALL GetValue(table=table, key=key, VALUE=funcStr, stat=stat, &
                origin=origin, isFound=isok, isScalar=isScalar)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    "Cannot find key "//key//" in the toml table")
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(funcStr)
  CALL AssertError1(isok, myName, &
                    "error in allocating funcStr while reading expression")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportVectorFuncStrFromToml

!----------------------------------------------------------------------------
!                                                ImportMatrixFuncStrFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportMatrixFuncStrFromToml(funcStr, table)
  TYPE(String), ALLOCATABLE, INTENT(INOUT) :: funcStr(:, :)
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportVectorFuncStrFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  CHARACTER(*), PARAMETER :: key = "expression"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading expression from toml ...')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
#endif
! TODO: Implement GetValue in toml utility.
  ! CALL GetValue(table=table, key=key, VALUE=funcStr, stat=stat, &
  !               origin=origin, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    "Cannot find key "//key//" in the toml table")
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(funcStr)
  CALL AssertError1(isok, myName, &
                    "error in allocating funcStr while reading expression")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportMatrixFuncStrFromToml

!----------------------------------------------------------------------------
!                                            ReadScalarEquationParserFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadScalarEquationParserFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadScalarEquationParserFromToml()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: stat, origin, tsize
  CHARACTER(funcopt%maxVarLen), ALLOCATABLE :: vars(:)
  CHARACTER(funcopt%maxlen) :: funcStr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = obj%returnType == varopt%scalar
  IF (.NOT. isok) THEN

#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'returnType is not scalar, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  ! Read equationParser
  CALL GetValue( &
    table=table, key="equationParser", VALUE=obj%isEquationParser, &
    default_value=math%no, isFound=isok, stat=stat, origin=origin)

  IF (.NOT. isok) THEN

#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'equationParser not found, so nothing to do here')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  ! this code is executed when equationParser is true
  ! here we will read vars
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Calling ImportVarNamesFromToml() ...')
#endif

  CALL ImportVarNamesFromToml(vars=vars, table=table)

! Check if vars is allocated
#ifdef DEBUG_VER
  isok = ALLOCATED(vars)
  CALL AssertError1(isok, myName, &
                    "vars read from toml is not allocated")
#endif

  ! Check the size of vars should be same as the numArgs
#ifdef DEBUG_VER
  tsize = SIZE(vars)
  CALL AssertError2(tsize, obj%numArgs, myName, &
                    "a=Size(vars) read from toml, b=obj%numArgs")
#endif

  ! if equationParser is true then read expression
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Calling ImportScalarFuncStrFromToml() ...')
#endif

  CALL ImportScalarFuncStrFromToml(funcStr=funcStr, table=table)

  CALL obj%SetScalarEquationParser(funcStr=funcStr, var=vars)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadScalarEquationParserFromToml

!----------------------------------------------------------------------------
!                                            ReadVectorEquationParserFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadVectorEquationParserFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadVectorEquationParserFromToml()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: stat, origin, tsize
  CHARACTER(funcopt%maxVarLen), ALLOCATABLE :: vars(:)
  TYPE(String), ALLOCATABLE :: funcStr(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = obj%returnType == varopt%vector
  IF (.NOT. isok) THEN

#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'returnType is not vector, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  ! Read equationParser
  CALL GetValue( &
    table=table, key="equationParser", VALUE=obj%isEquationParser, &
    default_value=math%no, isFound=isok, stat=stat, origin=origin)

  IF (.NOT. isok) THEN

#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'equationParser not found, so nothing to do here')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  ! this code is executed when equationParser is true
  ! here we will read vars
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Calling ImportVarNamesFromToml() ...')
#endif

  CALL ImportVarNamesFromToml(vars=vars, table=table)

! Check if vars is allocated
#ifdef DEBUG_VER
  isok = ALLOCATED(vars)
  CALL AssertError1(isok, myName, &
                    "vars read from toml is not allocated")
#endif

  ! Check the size of vars should be same as the numArgs
#ifdef DEBUG_VER
  tsize = SIZE(vars)
  CALL AssertError2(tsize, obj%numArgs, myName, &
                    "a=Size(vars) read from toml, b=obj%numArgs")
#endif

  ! if equationParser is true then read expression
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Calling ImportScalarFuncStrFromToml() ...')
#endif

  CALL ImportVectorFuncStrFromToml(funcStr=funcStr, table=table)

  CALL obj%SetVectorEquationParser(funcStr=funcStr, var=vars)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadVectorEquationParserFromToml

!----------------------------------------------------------------------------
!                                           ReadMatrixEquationParserFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadMatrixEquationParserFromToml(obj, table)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadMatrixEquationParserFromToml()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: stat, origin, tsize
  CHARACTER(funcopt%maxVarLen), ALLOCATABLE :: vars(:)
  TYPE(String), ALLOCATABLE :: funcStr(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isok = obj%returnType == varopt%matrix
  IF (.NOT. isok) THEN

#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'returnType is not matrix, so nothing to do here.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  ! Read equationParser
  CALL GetValue( &
    table=table, key="equationParser", VALUE=obj%isEquationParser, &
    default_value=math%no, isFound=isok, stat=stat, origin=origin)

  IF (.NOT. isok) THEN

#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'equationParser not found, so nothing to do here')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  ! this code is executed when equationParser is true
  ! here we will read vars
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Calling ImportVarNamesFromToml() ...')
#endif

  CALL ImportVarNamesFromToml(vars=vars, table=table)

! Check if vars is allocated
#ifdef DEBUG_VER
  isok = ALLOCATED(vars)
  CALL AssertError1(isok, myName, &
                    "vars read from toml is not allocated")
#endif

  ! Check the size of vars should be same as the numArgs
#ifdef DEBUG_VER
  tsize = SIZE(vars)
  CALL AssertError2(tsize, obj%numArgs, myName, &
                    "a=Size(vars) read from toml, b=obj%numArgs")
#endif

  ! if equationParser is true then read expression
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Calling ImportScalarFuncStrFromToml() ...')
#endif

  CALL ImportMatrixFuncStrFromToml(funcStr=funcStr, table=table)

  CALL obj%SetMatrixEquationParser(funcStr=funcStr, var=vars)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadMatrixEquationParserFromToml

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

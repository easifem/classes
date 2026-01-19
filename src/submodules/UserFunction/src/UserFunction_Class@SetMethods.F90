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

SUBMODULE(UserFunction_Class) SetMethods
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE EquationParser_Class, ONLY: EquationParser_Pointer
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%name = name

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetName

!----------------------------------------------------------------------------
!                                                  SetScalarFunctionPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetScalarFunctionPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetScalarFunctionPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError2(obj%returnType, varopt%scalar, myName, &
                  'a=obj%returnType and b=scalar')
#endif

obj%isExternalFunc = math%yes
obj%scalarFunction => func
obj%engineID = funcopt%externalEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetScalarFunctionPointer

!----------------------------------------------------------------------------
!                                                  SetVectorFunctionPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetVectorFunctionPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetVectorFunctionPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError2(obj%returnType, varopt%vector, myName, &
                  'a=obj%returnType and b=vector')
#endif

obj%isExternalFunc = math%yes
obj%vectorFunction => func
obj%engineID = funcopt%externalEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetVectorFunctionPointer

!----------------------------------------------------------------------------
!                                                   SetMatrixFunctionPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMatrixFunctionPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMatrixFunctionPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError2(obj%returnType, varopt%matrix, myName, &
                  'a=obj%returnType and b=matrix')
#endif

obj%isExternalFunc = math%yes
obj%matrixFunction => func
obj%engineID = funcopt%externalEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMatrixFunctionPointer

!----------------------------------------------------------------------------
!                                                                SetLuaScript
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLuaScript
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetLuaScript()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, &
                  'UserFunction_::obj is not initiated.')
#endif

obj%isLuaScript = math%yes
obj%luaScript = luaScript
obj%luaFunctionName = luaFunctionName
obj%engineID = funcopt%luaEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_SetLuaScript

!----------------------------------------------------------------------------
!                                                        SetScalarConstantVal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetScalarConstantVal
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetScalarConstantVal()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, &
                  'UserFunction_::obj is not initiated.')
#endif

#ifdef DEBUG_VER
CALL AssertError2(obj%returnType, varopt%scalar, myName, &
                  'a=obj%returnType and b=scalar')
#endif

obj%scalarValue = val
obj%engineID = funcopt%constEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_SetScalarConstantVal

!----------------------------------------------------------------------------
!                                                        SetVectorConstantVal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetVectorConstantVal
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetVectorConstantVal()"
INTEGER(I4B) :: tsize
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, &
                  'UserFunction_::obj is not initiated.')
#endif

#ifdef DEBUG_VER
CALL AssertError2(obj%returnType, varopt%vector, myName, &
                  'a=obj%returnType and b=vector')
#endif

#ifdef DEBUG_VER
tsize = SIZE(val)
CALL AssertError3(obj%numReturns, tsize, myName, &
                  'a = obj%numReturns and b=size(val)')
#endif

obj%vectorValue(1:obj%numReturns) = val(1:obj%numReturns)
obj%engineID = funcopt%constEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_SetVectorConstantVal

!----------------------------------------------------------------------------
!                                                        SetMatrixConstantVal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMatrixConstantVal
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMatrixConstantVal()"
INTEGER(I4B) :: myshape(2)
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, &
                  'UserFunction_::obj is not initiated.')
#endif

#ifdef DEBUG_VER
CALL AssertError2(obj%returnType, varopt%matrix, myName, &
                  'a=obj%argType and b=matrix')
#endif

#ifdef DEBUG_VER
myshape = SHAPE(val)
CALL AssertError3(obj%returnShape(1), myshape(1), myName, &
                  'shape mismatch: a=obj%returnShape(1) and b=rows of val')

CALL AssertError3(obj%returnShape(2), myshape(2), myName, &
                  'shape mismatch: a=obj%returnShape(2) and b=cols of val')
#endif

obj%matrixValue(1:obj%returnShape(1), 1:obj%returnShape(2)) = &
  val(1:obj%returnShape(1), 1:obj%returnShape(2))

obj%engineID = funcopt%constEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_SetMatrixConstantVal

!----------------------------------------------------------------------------
!                                                     SetScalarEquationParser
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetScalarEquationParser
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetScalarEquationParser()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%scalarEqParser%Initiate(funcStr=funcStr, var=var)
obj%engineID = funcopt%equationParserEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetScalarEquationParser

!----------------------------------------------------------------------------
!                                                    SetVectorEquationParser
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetVectorEquationParser
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVectorEquationParser()"
#endif

INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
INTEGER(I4B) :: tfuncStrs
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
tfuncStrs = SIZE(funcStr)
CALL AssertError2(tfuncStrs, obj%numReturns, myName, &
                  "a=size(funcStr), b=obj%numReturns")
#endif

#ifdef DEBUG_VER
CALL AssertError3(tfuncStrs, funcopt%vectorFuncNumReturns, myName, &
                  "a=size(funcStr), b=obj%vectorFuncNumReturns")
#endif

DO ii = 1, obj%numReturns
  isok = ASSOCIATED(obj%vectorEqParser(ii)%ptr)
  IF (isok) CALL obj%vectorEqParser(ii)%ptr%DEALLOCATE()

  obj%vectorEqParser(ii)%ptr => EquationParser_Pointer( &
                                funcStr=funcStr(ii)%chars(), var=var)
END DO

obj%engineID = funcopt%equationParserEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetVectorEquationParser

!----------------------------------------------------------------------------
!                                                    SetMatrixEquationParser
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMatrixEquationParser
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMatrixEquationParser()"
#endif

INTEGER(I4B) :: ii, jj
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
INTEGER(I4B) :: funcSize
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
funcSize = SIZE(funcStr, 1)
CALL AssertError2(obj%returnShape(1), funcSize, myName, &
                  "a=obj%returnShape(1), a=Rows in funcStr")
#endif

#ifdef DEBUG_VER
funcSize = SIZE(funcStr, 2)
CALL AssertError2(obj%returnShape(2), funcSize, myName, &
                  "a=obj%returnShape(2), b=Cols in funcStr")
#endif

DO jj = 1, obj%returnShape(2)
  DO ii = 1, obj%returnShape(1)
    isok = ASSOCIATED(obj%matrixEqParser(ii, jj)%ptr)
    IF (isok) CALL obj%matrixEqParser(ii, jj)%ptr%DEALLOCATE()

    obj%matrixEqParser(ii, jj)%ptr => EquationParser_Pointer( &
                                     funcStr=funcStr(ii, jj)%chars(), var=var)
  END DO
END DO

obj%engineID = funcopt%equationParserEngine

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMatrixEquationParser

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

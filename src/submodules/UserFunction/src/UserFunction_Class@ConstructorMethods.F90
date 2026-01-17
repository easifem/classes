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

SUBMODULE(UserFunction_Class) ConstructorMethods
USE StringUtility, ONLY: UpperCase
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                  UserFunctionGetReturnType
!----------------------------------------------------------------------------

MODULE PROCEDURE UserFunctionGetReturnType
CHARACTER(1) :: name0
name0 = UpperCase(name(1:1))

SELECT CASE (name0)
CASE ("S")
  ans = varopt%Scalar
CASE ("V")
  ans = varopt%Vector
CASE ("M")
  ans = varopt%Matrix
CASE DEFAULT
  ans = -1
END SELECT
END PROCEDURE UserFunctionGetReturnType

!----------------------------------------------------------------------------
!                                                      UserFunctionGetArgType
!----------------------------------------------------------------------------

MODULE PROCEDURE UserFunctionGetArgType
CHARACTER(2) :: name0
INTEGER(I4B) :: n

name0 = UpperCase(name(1:2))

SELECT CASE (name0)
CASE ("CO")
  ans = varopt%Constant
CASE ("TI")
  ans = varopt%Time
CASE ("OT", "SO")
  ans = varopt%SolutionDependent
CASE ("SP")
  n = LEN_TRIM(name)
  IF (n > 5) THEN
    ans = varopt%SpaceTime
  ELSE
    ans = varopt%Space
  END IF
CASE DEFAULT
  ans = -1
END SELECT
END PROCEDURE UserFunctionGetArgType

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
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%isExternalFunc = .FALSE.
obj%isLuaScript = .FALSE.
obj%luaScript = ""
obj%luaFunctionName = ""
obj%returnType = 0
obj%returnShape = 0
obj%argType = 0
obj%numArgs = 0
obj%numReturns = 0
obj%scalarValue = 0.0_DFP
obj%name = ""
obj%vectorValue = 0.0_DFP
obj%matrixValue = 0.0_DFP
obj%scalarFunction => NULL()
obj%vectorFunction => NULL()
obj%matrixFunction => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.

obj%name = name
obj%returnType = returnType
obj%argType = argType

! Handle optional arguments numArgs
isok = PRESENT(numArgs)
IF (isok) THEN
  obj%numArgs = numArgs
ELSE
  obj%numArgs = GetDefaultNumArgs(argType)
END IF

! Handle optional arguments numReturns
! obj%numReturns = numReturns
isok = PRESENT(numReturns)
IF (isok) THEN
  obj%numReturns = numReturns
ELSE
  obj%numReturns = GetDefaultNumReturns(returnType)
END IF

! obj%returnShape = returnShape
#ifdef DEBUG_VER
isok = obj%returnType == varopt%matrix
IF (isok) THEN
  isok = PRESENT(returnShape)
  CALL AssertError1(isok, myName, &
             'When returnType is Matrix, then returnShape should be present.')

  isok = obj%numReturns == (returnShape(1) * returnShape(2))
  CALL AssertError1(isok, myName, &
       'When returnType is Matrix, then numReturns should be equal to the &
       &total number of elements of returned matrix.')
END IF
#endif

#ifdef DEBUG_VER
isok = obj%returnType == varopt%scalar
IF (isok) THEN
  isok = obj%numReturns == 1
  CALL AssertError1(isok, myName, &
                    'When returnType is Scalar, then numReturns should be 1.')
END IF
#endif

isok = PRESENT(returnShape)
IF (isok) obj%returnShape = returnShape

obj%isLuaScript = PRESENT(luaScript)
IF (.NOT. obj%isLuaScript) THEN

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

obj%luaScript = luaScript

#ifdef DEBUG_VER
isok = PRESENT(luaFunctionName)
CALL AssertError1(isok, myName, &
           'When luaScript is given, then luaFunctionName should be present.')
#endif

obj%luaFunctionName = luaFunctionName

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_Vector()"
#endif
#include "../../include/deallocate_vector.F90"
END PROCEDURE obj_Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_Ptr_Vector()"
#endif
#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods

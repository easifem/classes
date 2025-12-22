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
USE FPL_Method, ONLY: CheckEssentialParam, Set, GetValue
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: Display, ToString

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
END SELECT

name0 = ""
END PROCEDURE UserFunctionGetReturnType

!----------------------------------------------------------------------------
!                                                  UserFunctionGetArgType
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
  IF (n .GT. 5) THEN
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
!                                                      SetUserFunctionParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetUserFunctionParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetUserFunctionParam()"
#endif

INTEGER(I4B) :: numArgs0, numReturns0, returnShape0(2)
LOGICAL(LGT) :: isok, abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Set(obj=param, dataType=1_I4B, prefix=myprefix, key="returnType", &
         VALUE=returnType)
CALL Set(obj=param, dataType=1_I4B, prefix=myprefix, key="argType", &
         VALUE=argType)
CALL Set(obj=param, dataType="char", prefix=myprefix, key="name", &
         VALUE=name)

isok = PRESENT(numArgs)

IF (isok) THEN
  numArgs0 = numArgs
ELSE
  numArgs0 = GetDefaultNumArgs(argType)
END IF

isok = PRESENT(numReturns)

IF (isok) THEN
  numReturns0 = numReturns
ELSE
  numReturns0 = GetDefaultNumReturns(returnType)
END IF

returnShape0 = 0

SELECT CASE (returnType)
CASE (varopt%Matrix)

#ifdef DEBUG_VER
  isok = PRESENT(returnShape)
  CALL AssertError1(isok, myName, &
             'When returnType is Matrix, then returnShape should be present.')
#endif

#ifdef DEBUG_VER
  isok = numReturns0 .EQ. returnShape(1) * returnShape(2)
  CALL AssertError1(isok, myName, &
       'When returnType is Matrix, then numReturns should be equal to the &
       &total number of elements of returned matrix.')
#endif

  returnShape0 = returnShape

CASE (varopt%Scalar)
  isok = numReturns0 .EQ. 1

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'When returnType is Scalar, then numReturns should be 1.')
#endif

END SELECT

CALL Set(obj=param, dataType=[1_I4B], prefix=myprefix, key="returnShape", &
         VALUE=returnShape0)

! CALL checerror_numargs(argType, numArgs0, myName)

CALL Set(obj=param, dataType=1_I4B, prefix=myprefix, key="numArgs", &
         VALUE=numArgs0)

CALL Set(obj=param, dataType=1_I4B, prefix=myprefix, key="numReturns", &
         VALUE=numReturns0)

abool = PRESENT(luaScript)

IF (abool) THEN

#ifdef DEBUG_VER
  isok = PRESENT(luaFunctionName)
  CALL AssertError1(isok, myName, &
      'When luaScript is given, you should also mention the luaFunctionName.')
#endif

  CALL Set(obj=param, dataType=.TRUE., prefix=myprefix, key="isLuaScript", &
           VALUE=.TRUE.)

  CALL Set(obj=param, dataType="char", prefix=myprefix, key="luaScript", &
           VALUE=luaScript)

  CALL Set(obj=param, dataType="char", prefix=myprefix, &
           key="luaFunctionName", VALUE=luaFunctionName)

ELSE

  CALL Set(obj=param, dataType=.TRUE., prefix=myprefix, key="isLuaScript", &
           VALUE=.FALSE.)

  CALL Set(obj=param, dataType="char", prefix=myprefix, key="luaScript", &
           VALUE="empty")

  CALL Set(obj=param, dataType="char", prefix=myprefix, &
           key="luaFunctionName", VALUE="empty")
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetUserFunctionParam

!----------------------------------------------------------------------------
!                                                        CheckEssentiaParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

astr = "/isLuaScript/luaScript/numReturns/numArgs/returnType/argType"// &
       "/luaFunctionName/returnShape/name"

CALL CheckEssentialParam(obj=param, &
                         keys=astr, &
                         prefix=myprefix, &
                         myName=myName, &
                         modName=modName)
!note: CheckEssentialParam param is defined in easifemClasses FPL_Method
astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_CheckEssentialParam

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
obj%isUserFunctionSet = .FALSE.
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
IF (ALLOCATED(obj%vectorValue)) DEALLOCATE (obj%vectorValue)
IF (ALLOCATED(obj%matrixValue)) DEALLOCATE (obj%matrixValue)
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
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
CALL obj%CheckEssentialParam(param)

CALL GetValue(obj=param, prefix=myprefix, key="name", VALUE=obj%name)

CALL GetValue(obj=param, prefix=myprefix, key="returnType", &
              VALUE=obj%returnType)

CALL GetValue(obj=param, prefix=myprefix, key="argType", VALUE=obj%argType)

CALL GetValue(obj=param, prefix=myprefix, key="isLuaScript", &
              VALUE=obj%isLuaScript)

CALL GetValue(obj=param, prefix=myprefix, key="luaScript", &
              VALUE=obj%luaScript)

CALL GetValue(obj=param, prefix=myprefix, key="luaFunctionName", &
              VALUE=obj%luaFunctionName)

CALL GetValue(obj=param, prefix=myprefix, key="numArgs", &
              VALUE=obj%numArgs)

CALL GetValue(obj=param, prefix=myprefix, key="numReturns", &
              VALUE=obj%numReturns)

CALL GetValue(obj=param, prefix=myprefix, key="returnShape", &
              VALUE=obj%returnShape)
obj%isInit = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
LOGICAL(LGT) :: abool
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
abool = obj%returnType .EQ. varopt%matrix
IF (abool) THEN
  isok = PRESENT(returnShape)
  CALL AssertError1(isok, myName, &
             'When returnType is Matrix, then returnShape should be present.')

  isok = obj%numReturns .EQ. returnShape(1) * returnShape(2)
  CALL AssertError1(isok, myName, &
       'When returnType is Matrix, then numReturns should be equal to the &
       &total number of elements of returned matrix.')
END IF
#endif

#ifdef DEBUG_VER
abool = obj%returnType .EQ. varopt%scalar
IF (abool) THEN
  isok = obj%numReturns .EQ. 1
  CALL AssertError1(isok, myName, &
                    'When returnType is Scalar, then numReturns should be 1.')
END IF
#endif

IF (PRESENT(returnShape)) obj%returnShape = returnShape

! obj%isLuaScript = isLuaScript
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
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                         checkerror_numargs
!----------------------------------------------------------------------------

SUBROUTINE checerror_numargs(argType, numArgs0, myname)
  INTEGER(I4B), INTENT(IN) :: argType, numArgs0
  CHARACTER(*), INTENT(IN) :: myName

  LOGICAL(LGT) :: isok

  SELECT CASE (argType)
  CASE (varopt%Constant)

    isok = numArgs0 .EQ. 0
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                       '[CONFIG ERROR] :: When argType is Constant, then '// &
                        'numArgs should be 0.')
      RETURN
    END IF

  CASE (varopt%Time)

    isok = numArgs0 .EQ. 1
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[CONFIG ERROR] :: When argType is Time, then '// &
                        'numArgs should be 1.')
      RETURN
    END IF

  CASE (varopt%Space)

    isok = numArgs0 .EQ. 3
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[CONFIG ERROR] :: When argType is Space, then '// &
                        'numArgs should be 3.')
      RETURN
    END IF

  CASE (varopt%SpaceTime)

    isok = numArgs0 .EQ. 4
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: When argType is SpaceTime, then '// &
                        'numArgs should be 4.')
      RETURN
    END IF

  END SELECT
END SUBROUTINE checerror_numargs

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

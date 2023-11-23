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
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                  UserFunctionGetReturnType
!----------------------------------------------------------------------------

MODULE PROCEDURE UserFunctionGetReturnType
TYPE(String) :: name0
name0 = UpperCase(name)
SELECT CASE (name0%chars())
CASE ("SCALAR")
  ans = Scalar
CASE ("VECTOR")
  ans = Vector
CASE ("MATRIX")
  ans = Matrix
END SELECT

name0 = ""
END PROCEDURE UserFunctionGetReturnType

!----------------------------------------------------------------------------
!                                                  UserFunctionGetArgType
!----------------------------------------------------------------------------

MODULE PROCEDURE UserFunctionGetArgType
TYPE(String) :: name0
name0 = UpperCase(name)
SELECT CASE (name0%chars())
CASE ("CONSTANT")
  ans = Constant
CASE ("SPACE")
  ans = Space
CASE ("SPACETIME")
  ans = SpaceTime
CASE ("OTHERS", "SOLUTIONDEPENDENT")
  ans = SolutionDependent
END SELECT
name0 = ""
END PROCEDURE UserFunctionGetArgType

!----------------------------------------------------------------------------
!                                                      SetUserFunctionParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetUserFunctionParam
CHARACTER(*), PARAMETER :: myName = "SetUserFunctionParam()"
INTEGER(I4B) :: numArgs0, numReturns0, returnShape0(2)

CALL Set(obj=param, dataType=1_I4B, prefix=myprefix, key="returnType",  &
  & VALUE=returnType)
CALL Set(obj=param, dataType=1_I4B, prefix=myprefix, key="argType",  &
  & VALUE=argType)
CALL Set(obj=param, dataType="char", prefix=myprefix, key="name",  &
  & VALUE=name)

IF (PRESENT(numArgs)) THEN
  numArgs0 = numArgs
ELSE
  SELECT CASE (argType)
  CASE (Constant)
    numArgs0 = DEFAULT_NUM_ARG_CONSTANT
  CASE (Space)
    numArgs0 = DEFAULT_NUM_ARG_SPACE
  CASE (Time)
    numArgs0 = DEFAULT_NUM_ARG_TIME
  CASE (SpaceTime)
    numArgs0 = DEFAULT_NUM_ARG_SPACETIME
  CASE DEFAULT
    numArgs0 = 0
  END SELECT
END IF

IF (PRESENT(numReturns)) THEN
  numReturns0 = numReturns
ELSE
  SELECT CASE (returnType)
  CASE (Scalar)
    numReturns0 = DEFAULT_NUM_ARG_SCALAR
  CASE (Vector)
    numReturns0 = DEFAULT_NUM_ARG_VECTOR
  CASE (Matrix)
    numReturns0 = DEFAULT_NUM_ARG_MATRIX
  CASE DEFAULT
    numReturns0 = 0
  END SELECT
END IF

returnShape0 = 0
IF (returnType .EQ. Matrix) THEN
  IF (.NOT. PRESENT(returnShape)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: When returnType is Matrix, then'//  &
      & CHAR_LF//'returnShape should be present.')
    RETURN
  END IF

  IF (numReturns0 .NE. returnShape(1) * returnShape(2)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: When returnType is Matrix, then '//  &
      & 'numReturns should be equal to the total number of elements.')
    RETURN
  END IF

  returnShape0 = returnShape
END IF

CALL Set(obj=param, dataType=[1_I4B], prefix=myprefix,  &
  & key="returnShape", VALUE=returnShape0)

IF (returnType .EQ. Scalar) THEN
  IF (numReturns0 .NE. 1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: When returnType is Scalar, then '//  &
      & 'numReturns should be 1.')
    RETURN
  END IF
END IF

IF (argType .EQ. Constant) THEN
  IF (numArgs .NE. 0) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: When argType is Constant, then '//  &
      & 'numArgs should be 0.')
    RETURN
  END IF
END IF

IF (argType .EQ. Time) THEN
  IF (numArgs .NE. 1) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: When argType is Time, then '//  &
      & 'numArgs should be 1.')
    RETURN
  END IF
END IF

IF (argType .EQ. Space) THEN
  IF (numArgs .NE. 3) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: When argType is Space, then '//  &
      & 'numArgs should be 3.')
    RETURN
  END IF
END IF

IF (argType .EQ. SpaceTime) THEN
  IF (numArgs .NE. SpaceTime) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: When argType is SpaceTime, then '//  &
      & 'numArgs should be 4.')
    RETURN
  END IF
END IF

CALL Set(obj=param, dataType=1_I4B, prefix=myprefix, key="numArgs",  &
  & VALUE=numArgs0)

CALL Set(obj=param, dataType=1_I4B, prefix=myprefix, key="numReturns",  &
  & VALUE=numReturns0)

IF (PRESENT(luaScript)) THEN
  IF (.NOT. PRESENT(luaFunctionName)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: When luaScript is given, you should'//  &
      & ' also mention the luaFunctionName.')
    RETURN
  END IF

  CALL Set(obj=param, dataType=.TRUE., prefix=myprefix, key="isLuaScript",  &
    & VALUE=.TRUE.)
  CALL Set(obj=param, dataType="char", prefix=myprefix, key="luaScript",  &
    & VALUE=luaScript)
  CALL Set(obj=param, dataType="char", prefix=myprefix,  &
    & key="luaFunctionName", VALUE=luaFunctionName)
ELSE
  CALL Set(obj=param, dataType=.TRUE., prefix=myprefix, key="isLuaScript",  &
    & VALUE=.FALSE.)
  CALL Set(obj=param, dataType="char", prefix=myprefix, key="luaScript",  &
    & VALUE="empty")
  CALL Set(obj=param, dataType="char", prefix=myprefix,  &
    & key="luaFunctionName", VALUE="empty")
END IF

END PROCEDURE SetUserFunctionParam

!----------------------------------------------------------------------------
!                                                        CheckEssentiaParam
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "auf_CheckEssentialParam()"
INTEGER(I4B) :: ii
TYPE(String), ALLOCATABLE :: essentialParam(:)
TYPE(String) :: astr

astr = "/isLuaScript/luaScript/numReturns/numArgs/returnType/argType"//  &
  &"/luaFunctionName/returnShape/name"

CALL astr%Split(essentialParam, sep="/")
CALL CheckEssentialParam(obj=param,  &
  & keys=essentialParam,  &
  & prefix=myprefix,  &
  & myName=myName,  &
  & modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method

IF (ALLOCATED(essentialParam)) THEN
  DO ii = 1, SIZE(essentialParam)
    essentialParam(ii) = ""
  END DO
  DEALLOCATE (essentialParam)
END IF
astr = ""
END PROCEDURE auf_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Deallocate
!> main
obj%isInitiated = .FALSE.
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
IF (ASSOCIATED(obj%scalarFunction)) obj%scalarFunction => NULL()
IF (ASSOCIATED(obj%vectorFunction)) obj%vectorFunction => NULL()
IF (ASSOCIATED(obj%matrixFunction)) obj%matrixFunction => NULL()
END PROCEDURE auf_Deallocate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Final
CALL obj%DEALLOCATE()
END PROCEDURE auf_Final

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Initiate
CALL obj%DEALLOCATE()
CALL obj%CheckEssentialParam(param)

CALL GetValue(obj=param, prefix=myprefix, key="name",  &
  & VALUE=obj%name)

CALL GetValue(obj=param, prefix=myprefix, key="returnType",  &
  & VALUE=obj%returnType)

CALL GetValue(obj=param, prefix=myprefix, key="argType",  &
  & VALUE=obj%argType)

CALL GetValue(obj=param, prefix=myprefix, key="isLuaScript",  &
  & VALUE=obj%isLuaScript)

CALL GetValue(obj=param, prefix=myprefix, key="luaScript",  &
  & VALUE=obj%luaScript)

CALL GetValue(obj=param, prefix=myprefix, key="luaFunctionName",  &
  & VALUE=obj%luaFunctionName)

CALL GetValue(obj=param, prefix=myprefix, key="numArgs",  &
  & VALUE=obj%numArgs)

CALL GetValue(obj=param, prefix=myprefix, key="numReturns",  &
  & VALUE=obj%numReturns)

CALL GetValue(obj=param, prefix=myprefix, key="returnShape",  &
  & VALUE=obj%returnShape)
obj%isInitiated = .TRUE.
END PROCEDURE auf_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods

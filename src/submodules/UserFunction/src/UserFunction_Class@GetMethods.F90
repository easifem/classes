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

SUBMODULE(UserFunction_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_GetName
ans = obj%name%chars()
END PROCEDURE auf_GetName

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_GetScalarValue
LOGICAL(LGT) :: isOK, isNotOK
CHARACTER(*), PARAMETER :: myName = "auf_GetScalarValue()"
#ifdef USE_LUA
TYPE(C_PTR) :: l
INTEGER(I4B) :: rc, nargs, nresults, iarg
LOGICAL(LGT) :: isFunction
#endif

isOK = obj%returnType .EQ. Scalar
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: The user function is not configured for '//  &
    & ' returnType = Scalar')
  RETURN
END IF

val = obj%scalarValue

IF (obj%isUserFunctionSet) THEN
  isOK = ASSOCIATED(obj%scalarFunction)
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isUserFunctionSet is true '//  &
      & CHAR_LF//' but obj%scalarFunction is not ASSOCIATED.')
    RETURN
  END IF
  val = obj%scalarFunction(x=args)
END IF

#ifdef USE_LUA

IF (obj%isLuaScript) THEN
  nargs = obj%numArgs
  nresults = obj%numReturns

  isNotOK = nresults .NE. 1_I4B
  IF (isNotOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%numReturns should be 1'// &
      & CHAR_LF//' but it is '//tostring(obj%numReturns))
    RETURN
  END IF

  IF (PRESENT(args)) THEN
    isNotOK = nargs .NE. SIZE(args)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
        & tostring(obj%numArgs)//' ) should be same as size of args ('// &
        & CHAR_LF//tostring(SIZE(args))//').')
      RETURN
    END IF

  ELSE

    isNotOK = nargs .NE. 0_I4B
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
        & tostring(obj%numArgs)//' ) should be equal to 0 when '// &
        & CHAR_LF//'args is not present.')
      RETURN
    END IF
  END IF

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isFunction = lua_isfunction(l, -1) .EQ. 1

  IF (.NOT. isFunction) THEN
    CALL lua_close(l)
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'In the lua script'//obj%luaScript%chars()//  &
      & CHAR_LF//'lua function named '//obj%luaFunctionName%chars()//  &
      & CHAR_LF//' is not a function.')
    RETURN
  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)
  IF (rc .NE. lua_ok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'Some error occured while calling lua_pcall(); '//  &
      & CHAR_LF//tostring(rc))
    RETURN
  END IF
  val = REAL(lua_tonumber(l, -1), kind=DFP)
  CALL lua_pop(l, 1)
  CALL lua_close(l)
END IF

#else
IF (obj%isLuaScript) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: Currently  lua script cannot be used for '//  &
    & ' UserFunction. ')
  RETURN
END IF
#endif

END PROCEDURE auf_GetScalarValue

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_GetVectorValue
LOGICAL(LGT) :: isOK, isNotOK
CHARACTER(*), PARAMETER :: myName = "auf_GetVectorValue()"

#ifdef USE_LUA
TYPE(C_PTR) :: l
INTEGER(I4B) :: rc, nargs, nresults, iarg
LOGICAL(LGT) :: isFunction
#endif

isOK = obj%returnType .EQ. Vector
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: The user function is not configured for '//  &
    & ' returnType = Vector')
  RETURN
END IF

IF (ALLOCATED(obj%vectorValue)) THEN
  CALL Reallocate(val, obj%numReturns)
#ifdef USE_BLAS95
  CALL Copy(x=val, y=obj%vectorValue)
#else
  val = obj%vectorValue
#endif
END IF

IF (obj%isUserFunctionSet) THEN
  isOK = ASSOCIATED(obj%vectorFunction)
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isUserFunctionSet is true '//  &
      & CHAR_LF//' but obj%vectorFunction is not ASSOCIATED.')
    RETURN
  END IF
  CALL Reallocate(val, obj%numReturns)
  val = obj%vectorFunction(x=args)
END IF

#ifdef USE_LUA

IF (obj%isLuaScript) THEN
  nargs = obj%numArgs
  nresults = obj%numReturns

  CALL Reallocate(val, obj%numReturns)

  IF (PRESENT(args)) THEN
    isNotOK = nargs .NE. SIZE(args)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//  &
        & CHAR_LF//tostring(obj%numArgs)//  &
        & ' ) should be same as size of args ('// &
        & CHAR_LF//tostring(SIZE(args))//').')
      RETURN
    END IF

  ELSE

    isNotOK = nargs .NE. 0_I4B
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
        & tostring(obj%numArgs)//' ) should be equal to 0 when '// &
        & CHAR_LF//'args is not present.')
      RETURN
    END IF
  END IF

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isFunction = lua_isfunction(l, -1) .EQ. 1

  IF (.NOT. isFunction) THEN
    CALL lua_close(l)
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'In the lua script'//obj%luaScript%chars()//  &
      & CHAR_LF//'lua function named '//obj%luaFunctionName%chars()//  &
      & CHAR_LF//' is not a function.')
    RETURN
  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)
  IF (rc .NE. lua_ok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'Some error occured while calling lua_pcall(); '//  &
      & CHAR_LF//tostring(rc))
    RETURN
  END IF

  DO iarg = nresults, 1, -1
    val(iarg) = REAL(lua_tonumber(l, nresults + iarg - 1), kind=DFP)
  END DO
  CALL lua_pop(l, nresults)
  CALL lua_close(l)
END IF

#else
IF (obj%isLuaScript) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: Currently  lua script cannot be used for '//  &
    & ' UserFunction. ')
  RETURN
END IF
#endif

END PROCEDURE auf_GetVectorValue

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_GetMatrixValue
LOGICAL(LGT) :: isOK, isNotOK
CHARACTER(*), PARAMETER :: myName = "auf_GetMatrixValue()"
INTEGER(I4B) :: myshape(2)
#ifdef USE_LUA
TYPE(C_PTR) :: l
INTEGER(I4B) :: rc, nargs, nresults, iarg
LOGICAL(LGT) :: isFunction
REAL(DFP), ALLOCATABLE :: dummyvec(:)
#endif

isOK = obj%returnType .EQ. Matrix
IF (.NOT. isOK) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: The user function is not configured for '//  &
    & ' returnType = Vector')
  RETURN
END IF

IF (ALLOCATED(obj%matrixValue)) THEN
  myshape = SHAPE(obj%matrixValue)
  CALL Reallocate(val, myshape(1), myshape(2))
  val = obj%matrixValue
END IF

IF (obj%isUserFunctionSet) THEN
  isOK = ASSOCIATED(obj%matrixFunction)
  IF (.NOT. isOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isUserFunctionSet '//  &
      & CHAR_LF//'is true but obj%matrixFunction is not ASSOCIATED.')
    RETURN
  END IF
  CALL Reallocate(val, obj%returnShape(1), obj%returnShape(2))
  val = obj%matrixFunction(x=args)
END IF

#ifdef USE_LUA

IF (obj%isLuaScript) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP Progress]')
  nargs = obj%numArgs
  nresults = obj%numReturns

  CALL Reallocate(dummyvec, nresults)

  IF (PRESENT(args)) THEN
    isNotOK = nargs .NE. SIZE(args)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//  &
        & CHAR_LF//tostring(obj%numArgs)//  &
        & ' ) should be same as size of args ('// &
        & CHAR_LF//tostring(SIZE(args))//').')
      RETURN
    END IF

  ELSE

    isNotOK = nargs .NE. 0_I4B
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
        & tostring(obj%numArgs)//' ) should be equal to 0 when '// &
        & CHAR_LF//'args is not present.')
      RETURN
    END IF
  END IF

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isFunction = lua_isfunction(l, -1) .EQ. 1

  IF (.NOT. isFunction) THEN
    CALL lua_close(l)
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'In the lua script'//obj%luaScript%chars()//  &
      & CHAR_LF//'lua function named '//obj%luaFunctionName%chars()//  &
      & CHAR_LF//' is not a function.')
    RETURN
  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)
  IF (rc .NE. lua_ok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'//  &
      & CHAR_LF//'Some error occured while calling lua_pcall(); '//  &
      & CHAR_LF//tostring(rc))
    RETURN
  END IF

  DO iarg = nresults, 1, -1
    dummyvec(iarg) = REAL(lua_tonumber(l, nresults + iarg - 1), kind=DFP)
  END DO
  CALL lua_pop(l, nresults)
  CALL lua_close(l)

  CALL Reallocate(val, obj%returnShape(1), obj%returnShape(2))
  val = RESHAPE(dummyvec, obj%returnShape)
  IF (ALLOCATED(dummyvec)) THEN
    DEALLOCATE (dummyvec)
  END IF
END IF

#else
IF (obj%isLuaScript) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: Currently  lua script cannot be used for '//  &
    & ' UserFunction. ')
  RETURN
END IF
#endif

END PROCEDURE auf_GetMatrixValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_GetArgType
ans = obj%argType
END PROCEDURE auf_GetArgType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_GetReturnType
ans = obj%returnType
END PROCEDURE auf_GetReturnType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_GetNumReturns
ans = obj%numReturns
END PROCEDURE auf_GetNumReturns

!----------------------------------------------------------------------------
!                                                           GetReturnShape
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_GetReturnShape
ans = obj%returnShape
END PROCEDURE auf_GetReturnShape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods

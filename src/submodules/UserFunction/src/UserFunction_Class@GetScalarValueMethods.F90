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

SUBMODULE(UserFunction_Class) GetScalarValueMethods
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: ToString
USE ISO_C_BINDING, ONLY: C_PTR
USE LuaInterface

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                CheckError
!----------------------------------------------------------------------------

SUBROUTINE checkerror(obj, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok
  CHARACTER(*), PARAMETER :: myName = "checkerror()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%returnType .EQ. varopt%Scalar
  CALL AssertError1(isok, myName, &
                'The user function is not configured for returnType = Scalar')
#endif

#ifdef DEBUG_VER
  IF (obj%isUserFunctionSet) THEN
    isok = ASSOCIATED(obj%scalarFunction)
    CALL AssertError1(isok, myName, &
       'UserFunction_::obj%isUserFunctionSet is true but obj%scalarFunction &
       &is not ASSOCIATED.')
  END IF
#endif

  IF (obj%isLuaScript) THEN
    CALL checkerror_lua(obj=obj, args=args)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE checkerror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_LUA

SUBROUTINE checkerror_lua(obj, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok, isargs
  INTEGER(I4B) :: nargs, nresults
  CHARACTER(*), PARAMETER :: myName = "checkerror_lua()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  nargs = obj%numArgs
  nresults = obj%numReturns
#endif

#ifdef DEBUG_VER
  isok = nresults .EQ. 1_I4B
  CALL AssertError1(isok, myName, &
                    'UserFunction_::obj%numReturns should be 1 but it is '// &
                    ToString(nresults))
#endif

#ifdef DEBUG_VER
  isargs = PRESENT(args)
  IF (isargs) THEN
    isok = nargs .LE. SIZE(args)
    CALL AssertError1(isok, myName, &
                      'UserFunction_::numArgs('//ToString(obj%numArgs)// &
                     ') should be <= size of args ('//ToString(SIZE(args))// &
                      ').')
  END IF
#endif

#ifdef DEBUG_VER
  IF (.NOT. isargs) THEN

    isok = nargs .EQ. 0_I4B
    CALL AssertError1(isok, myName, &
                      'UserFunction_::numArgs('//ToString(obj%numArgs)// &
                      ') should be equal to 0 when args is not present.')
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE checkerror_lua

!----------------------------------------------------------------------------
!                                                           checkerror_lua
!----------------------------------------------------------------------------

#else

SUBROUTINE checkerror_lua(obj, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  LOGICAL(LGT), PARAMETER :: isok = .FALSE.
  CHARACTER(*), PARAMETER :: myName = "checkerror_lua()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'Currently  lua script cannot be used for UserFunction. ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE checkerror_lua

#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_LUA

SUBROUTINE getvalue_lua(obj, val, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "getvalue_lua()"
#endif

  LOGICAL(LGT) :: isok
  TYPE(C_PTR) :: l
  INTEGER(I4B) :: nargs, nresults, iarg, rc

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nargs = obj%numArgs
  nresults = obj%numReturns

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isok = lua_isfunction(l, -1) .EQ. 1

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                'UserFunction_::obj%isLuaScript is TRUE In the lua script'// &
                    obj%luaScript%chars()//'lua function named '// &
                    obj%luaFunctionName%chars()//' is not a function.')
#endif

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)

#ifdef DEBUG_VER
  isok = rc .EQ. lua_ok
  CALL AssertError1(isok, myName, &
                    'UserFunction_::obj%isLuaScript is TRUE Some error &
                    &occured while calling lua_pcall(); '//ToString(rc))
#endif

  val = REAL(lua_tonumber(l, -1), kind=DFP)
  CALL lua_pop(l, 1)
  CALL lua_close(l)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE getvalue_lua

#else

SUBROUTINE getvalue_lua(obj, val, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
END SUBROUTINE getvalue_lua

#endif

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetScalarValue
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetScalarValue()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL checkerror(obj=obj, args=args)
#endif

val = obj%scalarValue

IF (obj%isUserFunctionSet) THEN
  val = obj%scalarFunction(x=args)
  RETURN
END IF

IF (obj%isLuaScript) THEN
  CALL getvalue_lua(obj=obj, val=val, args=args)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetScalarValue

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetScalarValueMethods

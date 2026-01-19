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
USE Display_Method, ONLY: Display
USE ISO_C_BINDING, ONLY: C_PTR
USE LuaInterface, ONLY: lual_newstate
USE LuaInterface, ONLY: lual_openlibs
USE LuaInterface, ONLY: lual_dofile
USE LuaInterface, ONLY: lua_getglobal
USE LuaInterface, ONLY: lua_isfunction
USE LuaInterface, ONLY: lua_close
USE LuaInterface, ONLY: lua_pushnumber
USE LuaInterface, ONLY: lua_pcall
USE LuaInterface, ONLY: lua_ok
USE LuaInterface, ONLY: lua_tonumber
USE LuaInterface, ONLY: lua_pop
USE LuaInterface, ONLY: lua_close
USE LuaInterface, ONLY: lua_number

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                CheckError
!----------------------------------------------------------------------------

SUBROUTINE CheckError(obj, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok
  CHARACTER(*), PARAMETER :: myName = "CheckError()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError2(obj%returnType, varopt%scalar, myName, &
                    'a=obj%returnType, b=scalar')
#endif

#ifdef DEBUG_VER
  IF (obj%isExternalFunc) THEN
    isok = ASSOCIATED(obj%scalarFunction)
    CALL AssertError1(isok, myName, &
       'UserFunction_::obj%isUserFunctionSet is true but obj%scalarFunction &
       &is not ASSOCIATED.')
  END IF
#endif

  IF (obj%isLuaScript) THEN
    CALL CheckError_Lua(obj=obj, args=args)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckError

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_LUA

SUBROUTINE CheckError_Lua(obj, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError_Lua()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: tsize
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError2(obj%numReturns, math%one_i, myName, &
                    'a=obj%numReturns, b=1')
#endif

#ifdef DEBUG_VER
  isok = PRESENT(args)
  IF (isok) THEN
    tsize = SIZE(args)
    CALL AssertError3(obj%numArgs, tsize, myName, &
                      'a=obj%numArgs and b=size(args)')
  ELSE

    CALL AssertError2(obj%numArgs, math%zero_i, myName, &
                      'a=obj%numArgs and b=0')
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckError_Lua

!----------------------------------------------------------------------------
!                                                           checkerror_lua
!----------------------------------------------------------------------------

#else

SUBROUTINE CheckError_Lua(obj, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError_Lua()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'Currently lua script cannot be used for UserFunction.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE CheckError_Lua

#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_LUA

SUBROUTINE GetValue_Lua(obj, val, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetValue_Lua()"
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
  rc = lual_dofile(l, TRIM(obj%luaScript))
  rc = lua_getglobal(l, TRIM(obj%luaFunctionName))
  isok = lua_isfunction(l, -1) == 1

#ifdef DEBUG_VER
  CALL AssertError1( &
    isok, myName, &
    'UserFunction_::obj%isLuaScript is TRUE In the lua script'// &
    TRIM(obj%luaScript)//'lua function named '// &
    TRIM(obj%luaFunctionName)//' is not a function.')
#endif

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)

#ifdef DEBUG_VER
  CALL AssertError2(rc, lua_ok, myName, &
                    'a=lua_pcall(l, nargs, ...), b=lua_ok')
#endif

  val = REAL(lua_tonumber(l, -1), kind=DFP)
  CALL lua_pop(l, 1)
  CALL lua_close(l)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetValue_Lua

#else

SUBROUTINE GetValue_Lua(obj, val, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  val = 0.0_DFP
END SUBROUTINE GetValue_Lua

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
CALL CheckError(obj=obj, args=args)
#endif

SELECT CASE (obj%engineID)
CASE (funcopt%constEngine)
  val = obj%scalarValue

CASE (funcopt%externalEngine)
  CALL obj%scalarFunction(args=args, nargs=obj%numArgs, ans=val)

CASE (funcopt%specialFuncEngine)
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "engineID=specialFuncEngine is notsupported yet")
#endif

CASE (funcopt%luaEngine)
  CALL GetValue_Lua(obj=obj, val=val, args=args)

CASE (funcopt%equationParserEngine)
  val = obj%scalarEqParser%Evaluate(val=args(1:obj%numArgs))

CASE (funcopt%symengineEngine)
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "engineID=specialFuncEngine is notsupported yet")
#endif

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for given engineID")
#endif

END SELECT

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

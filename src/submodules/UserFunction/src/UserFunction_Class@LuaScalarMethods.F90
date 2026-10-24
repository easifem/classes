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

#ifdef USE_LUA
SUBMODULE(UserFunction_Class) LuaScalarMethods
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
USE LuaInterface, ONLY: lua_pushvalue
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "UserFunction_Class@LuaScalarMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CheckError_Scalar_Lua
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError_Scalar_Lua()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: tsize
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE CheckError_Scalar_Lua

!----------------------------------------------------------------------------
!                                                       GetScalarValue_Lua1
!----------------------------------------------------------------------------

MODULE PROCEDURE GetScalarValue_Lua1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetScalarValue_Lua1()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(C_PTR) :: l
  INTEGER(I4B) :: iarg, rc

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, TRIM(obj%luaScript))
  rc = lua_getglobal(l, TRIM(obj%luaFunctionName))
  isok = lua_isfunction(l, -1) == 1

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'UserFunction_::obj%isLuaScript is TRUE In the lua ' // &
                    'script' // TRIM(obj%luaScript) // &
                    'lua function named ' // TRIM(obj%luaFunctionName) // &
                    ' is not a function.')
#endif

  DO iarg = 1, obj%numArgs
    CALL lua_pushnumber(l, REAL(args(iarg), KIND=lua_number))
  END DO

  rc = lua_pcall(l, obj%numArgs, obj%numReturns, 0)

#ifdef DEBUG_VER
  CALL AssertError2(rc, lua_ok, myName, &
                    'a=lua_pcall(l, nargs, ...), b=lua_ok')
#endif

  val = REAL(lua_tonumber(l, -1), KIND=DFP)
  CALL lua_pop(l, 1)
  CALL lua_close(l)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE GetScalarValue_Lua1

!----------------------------------------------------------------------------
!                                                       GetScalarValue_Lua2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetScalarValue_Lua2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetScalarValue_Lua2()"
#endif

  INTEGER(I4B) :: ii
  LOGICAL(LGT) :: isok
  TYPE(C_PTR) :: l
  INTEGER(I4B) :: iarg, rc

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, TRIM(obj%luaScript))
  rc = lua_getglobal(l, TRIM(obj%luaFunctionName))

#ifdef DEBUG_VER
  isok = lua_isfunction(l, -1) == 1
  CALL AssertError1(isok, myName, &
                    'UserFunction_::obj%isLuaScript is TRUE In the lua ' // &
                    'script' // TRIM(obj%luaScript) // &
                    'lua function named ' // TRIM(obj%luaFunctionName) // &
                    ' is not a function.')
#endif

  DO ii = 1, tsize

#ifdef DEBUG_VER
    CALL CheckError_Scalar_Lua(obj=obj, args=args(:, ii))
#endif

    CALL lua_pushvalue(l, -1)

    DO iarg = 1, obj%numArgs
      CALL lua_pushnumber(l, REAL(args(iarg, ii), KIND=lua_number))
    END DO

    rc = lua_pcall(l, obj%numArgs, obj%numReturns, 0)

#ifdef DEBUG_VER
    CALL AssertError2(rc, lua_ok, myName, &
                      'a=lua_pcall(l, nargs, ...), b=lua_ok')
#endif

    val(ii) = REAL(lua_tonumber(l, -1), KIND=DFP)
    CALL lua_pop(l, 1)

  END DO

  CALL lua_close(l)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE GetScalarValue_Lua2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LuaScalarMethods

!----------------------------------------------------------------------------
!                                                           No Lua module
!----------------------------------------------------------------------------

#else

SUBMODULE(UserFunction_Class) LuaScalarMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "UserFunction_Class@LuaScalarMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE CheckError_Lua
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError_Lua()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          'You are seeing this method because ' // &
                          'easifemBase is build with' // &
                          ' USE_LUA flag set to FALSE. This method is ' // &
                          'not implemented in this case.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE CheckError_Lua

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetScalarValue_Lua1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetValue_Lua()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          'You are seeing this method because ' // &
                          'easifemBase is build with' // &
                          ' USE_LUA flag set to FALSE. This method is ' // &
                          'not implemented in this case.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE GetScalarValue_Lua1

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LuaScalarMethods
#endif

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
SUBMODULE(UserFunction_Class) LuaVectorMethods
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
CHARACTER(*), PARAMETER :: modName = "UserFunction_Class@LuaVectorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                           OpenSaveLuaState
!----------------------------------------------------------------------------

SUBROUTINE OpenSaveLuaState(obj)
  CLASS(UserFunction_), INTENT(INOUT) :: obj

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "OpenSaveLuaState()"
  LOGICAL(LGT) :: isok
#endif
  INTEGER(I4B) :: rc

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. obj%isLuaStateOpen) THEN
    obj%luaState = lual_newstate()
    CALL lual_openlibs(obj%luaState)
    rc = lual_dofile(obj%luaState, TRIM(obj%luaScript))
    rc = lua_getglobal(obj%luaState, TRIM(obj%luaFunctionName))

#ifdef DEBUG_VER
    isok = lua_isfunction(obj%luaState, -1) == 1

    ! if not a function close the lua script
    IF (.NOT. isok) THEN
      CALL lua_close(obj%luaState)
    END IF

    CALL AssertError1(isok, myName, &
                      'UserFunction_::obj%isLuaScript is TRUE In the lua ' &
                      // 'script' // TRIM(obj%luaScript) // &
                      'lua function named ' // TRIM(obj%luaFunctionName) // &
                      ' is not a function.')
#endif

    obj%isLuaStateOpen = math%yes
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE OpenSaveLuaState

!----------------------------------------------------------------------------
!                                                       GetScalarValue_Lua1
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVectorValue_Lua1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValue_Lua1()"
#endif
  INTEGER(I4B) :: iarg, rc

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL OpenSaveLuaState(obj=obj)

  CALL lua_pushvalue(obj%luaState, math%minus_one_i)

  DO iarg = 1, obj%numArgs
    CALL lua_pushnumber(obj%luaState, REAL(args(iarg), KIND=lua_number))
  END DO

  rc = lua_pcall(obj%luaState, obj%numArgs, obj%numReturns, math%zero_i)

#ifdef DEBUG_VER
  CALL AssertError2(rc, lua_ok, myName, &
                    'a=lua_pcall(l, nargs, ...), b=lua_ok')
#endif

  DO iarg = 1, obj%numReturns
    val(iarg) = REAL(lua_tonumber(obj%luaState, &
                                  iarg - obj%numReturns - 1), KIND=DFP)
  END DO

  CALL lua_pop(obj%luaState, obj%numReturns)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE GetVectorValue_Lua1

!----------------------------------------------------------------------------
!                                                       GetScalarValue_Lua2
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVectorValue_Lua2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValue_Lua2()"
#endif

  INTEGER(I4B) :: ii
  INTEGER(I4B) :: iarg, rc

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL OpenSaveLuaState(obj=obj)

  DO ii = 1, ncol

    CALL lua_pushvalue(obj%luaState, -1)

    DO iarg = 1, obj%numArgs
      CALL lua_pushnumber(obj%luaState, REAL(args(iarg, ii), KIND=lua_number))
    END DO

    rc = lua_pcall(obj%luaState, obj%numArgs, obj%numReturns, math%zero_i)

#ifdef DEBUG_VER
    CALL AssertError2(rc, lua_ok, myName, &
                      'a=lua_pcall(l, nargs, ...), b=lua_ok')
#endif

    DO iarg = 1, obj%numReturns
      val(iarg, ii) = REAL(lua_tonumber(obj%luaState, &
                                        iarg - obj%numReturns - 1), KIND=DFP)
    END DO

    CALL lua_pop(obj%luaState, obj%numReturns)

  END DO

  ! Keep the original function on the stack for the next batch.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE GetVectorValue_Lua2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LuaVectorMethods

!----------------------------------------------------------------------------
!                                                           No Lua module
!----------------------------------------------------------------------------

#else

SUBMODULE(UserFunction_Class) LuaVectorMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "UserFunction_Class@LuaVectorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVectorValue_Lua1
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
END PROCEDURE GetVectorValue_Lua1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVectorValue_Lua2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetVectorValue_Lua2()"
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
END PROCEDURE GetVectorValue_Lua2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LuaVectorMethods
#endif

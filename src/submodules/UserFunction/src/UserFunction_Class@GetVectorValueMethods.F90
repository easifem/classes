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

SUBMODULE(UserFunction_Class) GetVectorValueMethods
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: ToString
USE ISO_C_BINDING, ONLY: C_PTR
USE ReallocateUtility, ONLY: Reallocate
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
!                                                                 checkerror
!----------------------------------------------------------------------------

SUBROUTINE CheckError(obj, n, val, args, myName)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myName

  ! Internal variables
#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
  isok = obj%returnType == varopt%vector
  CALL AssertError1(isok, myName, &
                'The user function is not configured for returnType = Vector')
#endif

#ifdef DEBUG_VER
  isok = obj%numReturns == n
  CALL AssertError1(isok, myName, &
                    'The user function numReturns should be equal to n.')
#endif

#ifdef DEBUG_VER
  isok = n <= SIZE(obj%vectorValue)
  CALL AssertError1(isok, myName, &
            'The user function numReturns should be less than or equal to &
            &size of obj%vectorValue.')
#endif

#ifdef DEBUG_VER
  IF (obj%isUserFunctionSet) THEN
    isok = ASSOCIATED(obj%vectorFunction)
    CALL AssertError1(isok, myName, &
         'UserFunction_::obj%isUserFunctionSet is true but &
         &obj%vectorFunction is not ASSOCIATED.')
  END IF
#endif

#ifdef DEBUG_VER
  IF (obj%isLuaScript) THEN
    CALL checkerror_lua(obj=obj, n=n, val=val, args=args, myName=myName)
  END IF
#endif
END SUBROUTINE CheckError

!----------------------------------------------------------------------------
!                                                           checkerror
!----------------------------------------------------------------------------

#ifdef USE_LUA
SUBROUTINE CheckError_Lua(obj, n, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: nargs, nresults, tsize

  nargs = obj%numArgs
  nresults = obj%numReturns

  isok = PRESENT(args)
  IF (isok) THEN

    tsize = SIZE(args)
    isok = nargs == tsize
    CALL AssertError1( &
      isok, myName, &
      'UserFunction_::numArgs( '//ToString(obj%numArgs)//' ) should be same &
      &as size of args ('//ToString(tsize)//').')

  ELSE

    isok = nargs == 0_I4B
    CALL AssertError1( &
      isok, myName, &
      'UserFunction_::numArgs( '//ToString(obj%numArgs)// &
      ' ) should be equal to 0 when args is not present.')
  END IF

#endif
END SUBROUTINE CheckError_Lua

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#else

SUBROUTINE CheckError_Lua(obj, n, val, args, myName)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myName

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
          'This subroutine should not be called when USE_LUA is not defined.')
#endif

END SUBROUTINE CheckError_Lua

#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_LUA

SUBROUTINE GetValue_Lua(obj, n, val, args, myName)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myName

  LOGICAL(LGT) :: isok
  TYPE(C_PTR) :: l
  INTEGER(I4B) :: rc, nargs, nresults, iarg

  nargs = obj%numArgs
  nresults = obj%numReturns
  ! CALL Reallocate(val, obj%numReturns)

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isok = lua_isfunction(l, -1) == 1

  IF (.NOT. isok) THEN
    CALL lua_close(l)

#ifdef DEBUG_VER
    CALL AssertError1( &
      isok, myName, &
      'UserFunction_::obj%isLuaScript is TRUE'// &
      CHAR_LF//'In the lua script'//obj%luaScript%chars()// &
      CHAR_LF//'lua function named '//obj%luaFunctionName%chars()// &
      CHAR_LF//' is not a function.')
#endif

  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)

#ifdef DEBUG_VER
  isok = rc == lua_ok
  CALL AssertError1( &
    isok, myName, &
    'UserFunction_::obj%isLuaScript is TRUE. Some error occured while &
    &calling lua_pcall(); '//ToString(rc))
#endif

  DO iarg = 1, nresults
    val(iarg) = REAL(lua_tonumber(l, iarg), kind=DFP)
  END DO

  CALL lua_pop(l, nresults)
  CALL lua_close(l)
END SUBROUTINE GetValue_Lua

!----------------------------------------------------------------------------
! getvalue_lua
!----------------------------------------------------------------------------

#else

SUBROUTINE GetValue_Lua(obj, n, val, args, myName)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myName
END SUBROUTINE GetValue_Lua

#endif

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = "obj_GetVectorValue()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myname//' - '// &
                        '[START] ')
#endif

CALL Reallocate(val, obj%numReturns)
CALL obj%Get_(tsize=tsize, val=val, args=args)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myname//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVectorValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue_
CHARACTER(*), PARAMETER :: myName = "obj_GetVectorValue_()"

#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%numReturns

#ifdef DEBUG_VER
isok = SIZE(val) >= tsize
CALL AssertError1(isok, myName, &
             'The size of val should be greater than or equal to numReturns.')
#endif

#ifdef DEBUG_VER
CALL CheckError(obj=obj, n=tsize, val=val, args=args, myname=myname)
#endif

IF (obj%isUserFunctionSet) THEN
  val(1:tsize) = obj%vectorFunction(x=args)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (obj%isLuaScript) THEN
  CALL GetValue_Lua(obj=obj, n=tsize, val=val, args=args, myName=myName)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

val(1:tsize) = obj%vectorValue(1:tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVectorValue_

!----------------------------------------------------------------------------
!                                                               Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetVectorValueMethods

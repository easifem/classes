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

SUBROUTINE checkerror(obj, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok

  isok = obj%returnType .EQ. varopt%Scalar
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myname//' - '// &
            '[INTERNAL ERROR] :: The user function is not configured for '// &
                      ' returnType = Scalar')
    RETURN
  END IF

  IF (obj%isUserFunctionSet) THEN
    isok = ASSOCIATED(obj%scalarFunction)

    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myname//' - '// &
       '[INTERNAL ERROR] :: UserFunction_::obj%isUserFunctionSet is true '// &
                        CHAR_LF//' but obj%scalarFunction is not ASSOCIATED.')
      RETURN
    END IF
  END IF

  IF (obj%isLuaScript) THEN
    CALL checkerror_lua(obj=obj, args=args, myname=myname)
  END IF

END SUBROUTINE checkerror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_LUA

SUBROUTINE checkerror_lua(obj, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: nargs, nresults

  nargs = obj%numArgs
  nresults = obj%numReturns

  isok = nresults .EQ. 1_I4B
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myname//' - '// &
           '[INTERNAL ERROR] :: UserFunction_::obj%numReturns should be 1'// &
                      CHAR_LF//' but it is '//ToString(obj%numReturns))
    RETURN
  END IF

  IF (PRESENT(args)) THEN

    isok = nargs .LE. SIZE(args)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myname//' - '// &
                   '[INTERNAL ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
                   ToString(obj%numArgs)//' ) should be <= size of args ('// &
                        CHAR_LF//ToString(SIZE(args))//').')
      RETURN
    END IF

  ELSE

    isok = nargs .EQ. 0_I4B
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myname//' - '// &
                     '[CONFIG ERROR] :: UserFunction_::numArgs( '//CHAR_LF// &
                    ToString(obj%numArgs)//' ) should be equal to 0 when '// &
                        CHAR_LF//'args is not present.')
      RETURN
    END IF

  END IF

END SUBROUTINE checkerror_lua

!----------------------------------------------------------------------------
!                                                           checkerror_lua
!----------------------------------------------------------------------------

#else

SUBROUTINE checkerror_lua(obj, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok

  CALL e%RaiseError(modName//'::'//myname//' - '// &
                '[WIP ERROR] :: Currently  lua script cannot be used for '// &
                    ' UserFunction. ')
  RETURN

END SUBROUTINE checkerror_lua

#endif

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#ifdef USE_LUA

SUBROUTINE getvalue_lua(obj, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok
  TYPE(C_PTR) :: l
  INTEGER(I4B) :: nargs, nresults, iarg, rc

  nargs = obj%numArgs
  nresults = obj%numReturns

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, obj%luaScript%chars())
  rc = lua_getglobal(l, obj%luaFunctionName%chars())
  isok = lua_isfunction(l, -1) .EQ. 1

  IF (.NOT. isok) THEN
    CALL lua_close(l)
    CALL e%RaiseError(modName//'::'//myname//' - '// &
              '[INTERNAL ERROR] :: UserFunction_::obj%isLuaScript is TRUE'// &
                      CHAR_LF//'In the lua script'//obj%luaScript%chars()// &
               CHAR_LF//'lua function named '//obj%luaFunctionName%chars()// &
                      CHAR_LF//' is not a function.')
    RETURN
  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)

  isok = rc .EQ. lua_ok
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myname//' - '// &
              '[INTERNAL ERROR] :: UserFunction_::obj%isLuaScript is TRUE'// &
                 CHAR_LF//'Some error occured while calling lua_pcall(); '// &
                      CHAR_LF//ToString(rc))
    RETURN
  END IF

  val = REAL(lua_tonumber(l, -1), kind=DFP)
  CALL lua_pop(l, 1)
  CALL lua_close(l)

END SUBROUTINE getvalue_lua

#else

SUBROUTINE getvalue_lua(obj, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname
END SUBROUTINE getvalue_lua

#endif

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetScalarValue
CHARACTER(*), PARAMETER :: myname = "obj_GetScalarValue()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')

CALL checkerror(obj=obj, args=args, myname=myname)
#endif

val = obj%scalarValue

IF (obj%isUserFunctionSet) THEN
  val = obj%scalarFunction(x=args)
  RETURN
END IF

IF (obj%isLuaScript) THEN
  CALL getvalue_lua(obj=obj, val=val, args=args, myname=myname)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetScalarValue

END SUBMODULE GetScalarValueMethods

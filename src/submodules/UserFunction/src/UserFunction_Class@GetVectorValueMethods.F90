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
USE LuaInterface
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 checkerror
!----------------------------------------------------------------------------

SUBROUTINE checkerror(obj, n, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok

  isok = obj%returnType .EQ. varopt%vector
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myname//' - '// &
              '[CONFIG ERROR] :: The user function is not configured for '// &
                      ' returnType = Vector')
    RETURN
  END IF

  isok = obj%numReturns .EQ. n
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myname//' - '// &
                      '[CONFIG ERROR] :: error in numReturns.')
    RETURN
  END IF

  isok = ALLOCATED(obj%vectorValue)
  IF (isok) THEN
    isok = n .LE. SIZE(obj%vectorValue)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myname//' - '// &
                '[INTERNAL ERROR] :: n should be <= size of obj%vectorValue.')
      RETURN
    END IF
  END IF

  IF (obj%isUserFunctionSet) THEN

    isok = ASSOCIATED(obj%vectorFunction)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myname//' - '// &
         '[CONFIG ERROR] :: UserFunction_::obj%isUserFunctionSet is true '// &
                        CHAR_LF//' but obj%vectorFunction is not ASSOCIATED.')
      RETURN
    END IF

  END IF

  IF (obj%isLuaScript) THEN
    CALL checkerror_lua(obj=obj, n=n, val=val, args=args, myname=myname)
  END IF

END SUBROUTINE checkerror

!----------------------------------------------------------------------------
!                                                           checkerror
!----------------------------------------------------------------------------

#ifdef USE_LUA
SUBROUTINE checkerror_lua(obj, n, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: nargs, nresults

  nargs = obj%numArgs
  nresults = obj%numReturns

  IF (PRESENT(args)) THEN

    isok = nargs .EQ. SIZE(args)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myname//' - '// &
                        '[CONFIG ERROR] :: UserFunction_::numArgs( '// &
                        CHAR_LF//ToString(obj%numArgs)// &
                        ' ) should be same as size of args ('// &
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
!
!----------------------------------------------------------------------------

#else

SUBROUTINE checkerror_lua(obj, n, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: nargs, nresults

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

SUBROUTINE getvalue_lua(obj, n, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

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
  isok = lua_isfunction(l, -1) .EQ. 1

  IF (.NOT. isok) THEN
    CALL lua_close(l)
    CALL e%RaiseError(modName//'::'//myname//' - '// &
                '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'// &
                      CHAR_LF//'In the lua script'//obj%luaScript%chars()// &
               CHAR_LF//'lua function named '//obj%luaFunctionName%chars()// &
                      CHAR_LF//' is not a function.')
    RETURN
  END IF

  DO iarg = 1, nargs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, nargs, nresults, 0)

  IF (rc .NE. lua_ok) THEN
    CALL e%RaiseError(modName//'::'//myname//' - '// &
                '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'// &
                 CHAR_LF//'Some error occured while calling lua_pcall(); '// &
                      CHAR_LF//ToString(rc))
    RETURN
  END IF

  DO iarg = 1, nresults
    val(iarg) = REAL(lua_tonumber(l, iarg), kind=DFP)
  END DO

  CALL lua_pop(l, nresults)
  CALL lua_close(l)

END SUBROUTINE getvalue_lua

!----------------------------------------------------------------------------
! getvalue_lua
!----------------------------------------------------------------------------

#else

SUBROUTINE getvalue_lua(obj, n, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n
    !! number of return values
    !! it should be equal to obj%numReturns
  REAL(DFP), INTENT(INOUT) :: val(n)
    !! returned value
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

END SUBROUTINE getvalue_lua

#endif

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = "obj_GetVectorValue1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myname//' - '// &
                        '[START] ')
#endif

CALL Reallocate(val, obj%numReturns)
CALL obj%GetVectorValue(n=obj%numReturns, val=val, args=args)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myname//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVectorValue1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue
LOGICAL(LGT) :: isok
CHARACTER(*), PARAMETER :: myname = "obj_GetVectorValue()"

#ifdef DEBUG_VER
CALL checkerror(obj=obj, n=n, val=val, args=args, myname=myname)
#endif

isok = ALLOCATED(obj%vectorValue)
IF (isok) THEN
  val(1:n) = obj%vectorValue(1:n)
  RETURN
END IF

IF (obj%isUserFunctionSet) THEN
  val(1:n) = obj%vectorFunction(x=args)
  RETURN
END IF

IF (obj%isLuaScript) THEN
  CALL getvalue_lua(obj=obj, n=n, val=val, args=args, myname=myname)
  RETURN
END IF

END PROCEDURE obj_GetVectorValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetVectorValueMethods

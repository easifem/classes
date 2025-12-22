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

SUBMODULE(UserFunction_Class) GetMatrixValueMethods
USE BaseType, ONLY: varopt => TypeFEVariableOpt
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: ToString
USE ISO_C_BINDING, ONLY: C_PTR
USE LuaInterface
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 CheckError
!----------------------------------------------------------------------------

SUBROUTINE checkerror(obj, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  LOGICAL(LGT) :: isok

  isok = obj%returnType .EQ. varopt%matrix
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myname//' - '// &
              '[CONFIG ERROR] :: The user function is not configured for '// &
                      ' returnType = Vector')
    RETURN
  END IF

  IF (obj%isUserFunctionSet) THEN
    isok = ASSOCIATED(obj%matrixFunction)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myname//' - '// &
                 '[CONFIG ERROR] :: UserFunction_::obj%isUserFunctionSet '// &
                 CHAR_LF//'is true but obj%matrixFunction is not ASSOCIATED.')
      RETURN
    END IF
  END IF

  IF (obj%isLuaScript) THEN
    CALL checkerror_lua(obj=obj, val=val, args=args, myname=myname)
  END IF

END SUBROUTINE checkerror

!----------------------------------------------------------------------------
!                                                           checkerror
!----------------------------------------------------------------------------

#ifdef USE_LUA
SUBROUTINE checkerror_lua(obj, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  INTEGER(I4B) :: nargs, nresults
  LOGICAL(LGT) :: isok

  nargs = obj%numArgs
  nresults = obj%numReturns

  IF (PRESENT(args)) THEN
    isok = nargs .EQ. SIZE(args)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myname//' - '// &
                        '[CONFIG ERROR] :: UserFunction_::numArgs( '// &
     CHAR_LF//ToString(obj%numArgs)//' ) should be same as size of args ('// &
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

SUBROUTINE checkerror_lua(obj, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  CALL e%RaiseError(modName//'::'//myname//' - '// &
                '[WIP ERROR] :: Currently  lua script cannot be used for '// &
                    ' UserFunction. ')
  RETURN
END SUBROUTINE checkerror_lua

#endif

!----------------------------------------------------------------------------
!                                                               Getvalue
!----------------------------------------------------------------------------

#ifdef USE_LUA
SUBROUTINE getvalue_lua(obj, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname

  TYPE(C_PTR) :: l
  INTEGER(I4B) :: rc, nargs, nresults, iarg, s(2)
  LOGICAL(LGT) :: isok
  REAL(DFP), ALLOCATABLE :: dummyvec(:)

  nargs = obj%numArgs
  nresults = obj%numReturns

  CALL Reallocate(dummyvec, nresults)

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
  isok = rc .EQ. lua_ok
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myname//' - '// &
                '[CONFIG ERROR] :: UserFunction_::obj%isLuaScript is TRUE'// &
                 CHAR_LF//'Some error occured while calling lua_pcall(); '// &
                      CHAR_LF//ToString(rc))
    RETURN
  END IF

  DO iarg = 1, nresults
    dummyvec(iarg) = REAL(lua_tonumber(l, iarg), kind=DFP)
  END DO
  CALL lua_pop(l, nresults)
  CALL lua_close(l)

  s = obj%returnShape(1:2)
  CALL Reallocate(val, s(1), s(2))
  val(1:s(1), 1:s(2)) = RESHAPE(dummyvec, s)
  DEALLOCATE (dummyvec)

END SUBROUTINE getvalue_lua

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#else
SUBROUTINE getvalue_lua(obj, val, args, myname)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  CHARACTER(*), INTENT(IN) :: myname
END SUBROUTINE getvalue_lua

#endif

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMatrixValue
CHARACTER(*), PARAMETER :: myname = "obj_GetMatrixValue()"
INTEGER(I4B) :: s(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL checkerror(obj=obj, val=val, args=args, myname=myname)
#endif

IF (ALLOCATED(obj%matrixValue)) THEN
  s = SHAPE(obj%matrixValue)
  CALL Reallocate(val, s(1), s(2))
  val(1:s(1), 1:s(2)) = obj%matrixValue
  RETURN
END IF

IF (obj%isUserFunctionSet) THEN
  s = obj%returnShape(1:2)
  CALL Reallocate(val, obj%returnShape(1), obj%returnShape(2))
  val(1:s(1), 1:s(2)) = obj%matrixFunction(x=args)
  RETURN
END IF

IF (obj%isLuaScript) THEN
  CALL getvalue_lua(obj=obj, val=val, args=args, myname=myname)
  RETURN
END IF

END PROCEDURE obj_GetMatrixValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMatrixValueMethods

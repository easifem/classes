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
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMatrixValue
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = "obj_GetMatrixValue()"
#endif

INTEGER(I4B) :: nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%returnShape(1)
ncol = obj%returnShape(2)
CALL Reallocate(val, nrow, ncol)
CALL obj%Get_(val=val, nrow=nrow, ncol=ncol, args=args)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMatrixValue

!----------------------------------------------------------------------------
!                                                                       Get_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMatrixValue_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMatrixValue_()"
#endif

INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = 0
ncol = 0

#ifdef DEBUG_VER
CALL CheckError(obj=obj, val=val, args=args)
#endif

SELECT CASE (obj%engineID)

CASE (funcopt%constEngine)

#ifdef DEBUG_VER
  CALL CheckError_ConstEngine(obj=obj)
#endif
  nrow = obj%returnShape(1)
  ncol = obj%returnShape(2)
  val(1:nrow, 1:ncol) = obj%matrixValue(1:nrow, 1:ncol)

CASE (funcopt%externalEngine)

#ifdef DEBUG_VER
  CALL CheckError_ExternalEngine(obj=obj)
#endif
  CALL obj%matrixFunction(args=args, nargs=obj%numArgs, ans=val, &
                          nrow=nrow, ncol=ncol)

CASE (funcopt%specialFuncEngine)

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "specialFuncEngine Not supported yet.")
#endif

CASE (funcopt%luaEngine)

  CALL GetValue_LuaEngine(obj=obj, val=val, args=args, nrow=nrow, ncol=ncol)

CASE (funcopt%equationParserEngine)

#ifdef DEBUG_VER
  CALL CheckError_EquationParserEngine(obj=obj)
#endif

  nrow = obj%returnShape(1)
  ncol = obj%returnShape(2)

  DO jj = 1, ncol
    DO ii = 1, nrow
      val(ii, jj) = obj%matrixEqParser(ii, jj)%ptr%Evaluate( &
                    val=args(1:obj%numArgs))
    END DO
  END DO

CASE (funcopt%symengineEngine)

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "specialFuncEngine Not supported yet.")
#endif

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for engineID.")
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMatrixValue_

!----------------------------------------------------------------------------
!                                                                 CheckError
!----------------------------------------------------------------------------

SUBROUTINE CheckError(obj, val, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val(:, :)
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError()"
  INTEGER(I4B) :: tsize
  LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError2(obj%returnType, varopt%matrix, myName, &
                    "a=obj%returnType, b=matrix")
#endif

#ifdef DEBUG_VER
  tsize = SIZE(val, 1)
  CALL AssertError3(obj%returnShape(1), tsize, myName, &
                    'a=obj%returnShape(1), b=SIZE(val, 1)')
#endif

#ifdef DEBUG_VER
  tsize = SIZE(val, 2)
  CALL AssertError3(obj%returnShape(2), tsize, myName, &
                    'a=obj%returnShape(2), b=SIZE(val, 2)')
#endif

#ifdef DEBUG_VER
  isok = PRESENT(args)
  IF (isok) THEN
    tsize = SIZE(args)
    CALL AssertError3(obj%numArgs, tsize, myName, &
                      'a=obj%numArgs, b=SIZE(args)')

  ELSE

    CALL AssertError2(obj%numArgs, math%zero_i, myName, &
                      'a=obj%numArgs, b=0')
  END IF
#endif

#ifdef DEBUG_VER
  tsize = obj%returnShape(1) * obj%returnShape(2)
  CALL AssertError2(tsize, obj%numReturns, myName, &
                  'a=obj%returnShape(1)*obj%returnShape(2), b=obj%numReturns')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckError

!----------------------------------------------------------------------------
!                                                     CheckError_ConstEngine
!----------------------------------------------------------------------------

SUBROUTINE CheckError_ConstEngine(obj)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  !! User function

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError_ConstEngine()"
  INTEGER(I4B) :: tsize
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  tsize = SIZE(obj%matrixValue, 1)
  CALL AssertError3(obj%returnShape(1), tsize, myName, &
                    'a=obj%returnShape(1), b=SIZE(obj%matrixValue, 1)')
#endif

#ifdef DEBUG_VER
  tsize = SIZE(obj%matrixValue, 2)
  CALL AssertError3(obj%returnShape(2), tsize, myName, &
                    'a=obj%returnShape(2), b=SIZE(obj%matrixValue, 2)')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckError_ConstEngine

!----------------------------------------------------------------------------
!                                                   CheckError_ExternalEngine
!----------------------------------------------------------------------------

SUBROUTINE CheckError_ExternalEngine(obj)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  !! User function

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError_ExternalEngine()"
  LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  IF (obj%isExternalFunc) THEN
    isok = ASSOCIATED(obj%matrixFunction)
    CALL AssertError1(isok, myName, &
         'UserFunction_::obj%isExternalSet is true but &
         &obj%matrixFunction is not ASSOCIATED.')
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckError_ExternalEngine

!----------------------------------------------------------------------------
!                                            CheckError_EquationParserEngine
!----------------------------------------------------------------------------

SUBROUTINE CheckError_EquationParserEngine(obj)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  !! User function

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError_EquationParserEngine()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  DO jj = 1, obj%returnShape(2)
    DO ii = 1, obj%returnShape(1)
      isok = ASSOCIATED(obj%matrixEqParser(ii, jj)%ptr)
      CALL AssertError1( &
        isok, myName, &
        "matrixEqParser("//ToString(ii)//","//ToString(jj)// &
        ") is not ASSOCIATED.")
    END DO
  END DO
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckError_EquationParserEngine

!----------------------------------------------------------------------------
!                                                               Getvalue
!----------------------------------------------------------------------------

#ifdef USE_LUA
SUBROUTINE GetValue_LuaEngine(obj, val, nrow, ncol, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)

  ! Internal variables

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetValue_LuaEngine()"
#endif

  TYPE(C_PTR) :: l
  INTEGER(I4B) :: rc, iarg
  LOGICAL(LGT) :: isok
  REAL(DFP) :: dummyvec( &
               funcopt%matrixFuncNumReturns * funcopt%matrixFuncNumReturns)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  l = lual_newstate()
  CALL lual_openlibs(l)
  rc = lual_dofile(l, TRIM(obj%luaScript))
  rc = lua_getglobal(l, TRIM(obj%luaFunctionName))
  isok = lua_isfunction(l, -1) == 1

  IF (.NOT. isok) CALL lua_close(l)

#ifdef DEBUG_VER
  CALL AssertError1( &
    isok, myName, 'In the lua script'//TRIM(obj%luaScript)// &
    CHAR_LF//'lua function named '//TRIM(obj%luaFunctionName)// &
    CHAR_LF//' is not a function.')
#endif

  DO iarg = 1, obj%numArgs
    CALL lua_pushnumber(l, REAL(args(iarg), kind=lua_number))
  END DO

  rc = lua_pcall(l, obj%numArgs, obj%numReturns, 0)

#ifdef DEBUG_VER
  CALL AssertError2(rc, lua_ok, myName, 'a=rc, b=lua_ok')
#endif

  DO iarg = 1, obj%numReturns
    dummyvec(iarg) = REAL(lua_tonumber(l, iarg), kind=DFP)
  END DO

  CALL lua_pop(l, obj%numReturns)
  CALL lua_close(l)

  nrow = obj%returnShape(1)
  ncol = obj%returnShape(2)

  val(1:nrow, 1:ncol) = RESHAPE(dummyvec(1:obj%numReturns), obj%returnShape)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetValue_LuaEngine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#else
SUBROUTINE GetValue_LuaEngine(obj, val, nrow, ncol, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: val(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
END SUBROUTINE GetValue_LuaEngine

#endif

!----------------------------------------------------------------------------
!                                                               Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMatrixValueMethods

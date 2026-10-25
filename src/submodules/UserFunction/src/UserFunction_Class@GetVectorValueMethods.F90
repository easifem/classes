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

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
  "UserFunction_Class@GetVectorValueMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myname = "obj_GetVectorValue()"
#endif

  INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myname // ' - ' // &
                          '[START] ')
#endif

  CALL Reallocate(val, obj%numReturns)
  CALL obj%Get_(tsize=tsize, val=val, args=args)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myname // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetVectorValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue_1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetVectorValue_1()"
#endif
  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = obj%numReturns

#ifdef DEBUG_VER
  CALL CheckError(obj=obj, val=val, args=args)
#endif

  SELECT CASE (obj%engineID)

  CASE (funcopt%constEngine)
    val(1:tsize) = obj%vectorValue(1:tsize)

  CASE (funcopt%externalEngine)

#ifdef DEBUG_VER
    CALL CheckError_ExternalEngine(obj=obj)
#endif

    CALL obj%vectorFunction(args=args, nargs=obj%numArgs, ans=val, &
                            tsize=tsize)

  CASE (funcopt%specialFuncEngine)

#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, "specialFuncEngine Not supported yet.")
#endif

  CASE (funcopt%luaEngine)
    !! tsize is read only in the following
    !! so make sure tsize is set
    CALL GetVectorValue_Lua1(obj=obj, val=val, tsize=tsize, args=args)

  CASE (funcopt%equationParserEngine)

#ifdef DEBUG_VER
    CALL CheckError_EquationParserEngine(obj=obj)
#endif

    DO ii = 1, obj%numReturns
      val(ii) = obj%vectorEqParser(ii)%ptr%Evaluate(val=args(1:obj%numArgs))
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetVectorValue_1

!----------------------------------------------------------------------------
!                                                            GetVectorValue_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVectorValue_2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetVectorValue_2()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ncol = SIZE(args, 2)

  IF (obj%engineID .NE. funcopt%luaEngine) THEN
    DO ii = 1, ncol
      CALL obj%GetVectorValue_(val=val(:, ii), tsize=nrow, args=args(:, ii))
    END DO
  ELSE
    nrow = obj%numReturns
    CALL GetVectorValue_Lua2(obj=obj, val=val, ncol=ncol, args=args)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetVectorValue_2

!----------------------------------------------------------------------------
!                                                                 CheckError
!----------------------------------------------------------------------------

SUBROUTINE CheckError(obj, val, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  !! User function
  REAL(DFP), INTENT(INOUT) :: val(:)
  !! Returned value
  REAL(DFP), INTENT(IN), OPTIONAL :: args(:)
  !! Arguments

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckError()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: tsize
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError2(obj%returnType, varopt%vector, myName, &
                    'a=obj%returnType, b=vector')
#endif

#ifdef DEBUG_VER
  tsize = SIZE(val)
  CALL AssertError3(obj%numReturns, tsize, myName, &
                    'a=obj%numReturns, b=SIZE(val)')
#endif

#ifdef DEBUG_VER
  tsize = SIZE(obj%vectorValue)
  CALL AssertError3(obj%numReturns, tsize, myName, &
                    'a=obj%numReturns, b=SIZE(obj%vectorValue)')
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE CheckError

!----------------------------------------------------------------------------
!                                                     CheckError_ConstEngine
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  IF (obj%isExternalFunc) THEN
    isok = ASSOCIATED(obj%vectorFunction)
    CALL AssertError1(isok, myName, &
                      'UserFunction_::obj%isExternalSet is true but &
    &obj%vectorFunction is not ASSOCIATED.')
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  INTEGER(I4B) :: ii
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  DO ii = 1, obj%numReturns
    isok = ASSOCIATED(obj%vectorEqParser(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      "vectorEqParser(" // ToString(ii) // &
                      ") is not ASSOCIATED.")
  END DO
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE CheckError_EquationParserEngine

!----------------------------------------------------------------------------
!                                                               Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetVectorValueMethods

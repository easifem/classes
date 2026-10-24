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
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
  "UserFunction_Class@GetScalarValueMethods.F90"
#endif
CONTAINS

!----------------------------------------------------------------------------
!                                                                CheckError
!----------------------------------------------------------------------------

SUBROUTINE CheckError(obj, args)
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN), OPTIONAL :: args(:)

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok
  CHARACTER(*), PARAMETER :: myName = "CheckError()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
                      'UserFunction_::obj%isUserFunctionSet is true but ' // &
                      'obj%scalarFunction is not ASSOCIATED.')
  END IF
#endif

  IF (obj%isLuaScript) THEN
    CALL CheckError_Scalar_Lua(obj=obj, args=args)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE CheckError

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetScalarValue_1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetScalarValue_1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
    CALL GetScalarValue_Lua1(obj=obj, val=val, args=args)

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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetScalarValue_1

!----------------------------------------------------------------------------
!                                                             GetScalarValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetScalarValue_2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetScalarValue_2()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = SIZE(args, 2)

  IF (obj%engineID .NE. funcopt%luaEngine) THEN
    DO ii = 1, tsize
      CALL obj%GetScalarValue_(val=val(ii), args=args(:, ii))
    END DO
  ELSE
    ! In the following call tsize is read only;
    ! so make sure tsize is set, above
    CALL GetScalarValue_Lua2(obj=obj, val=val, tsize=tsize, args=args)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetScalarValue_2

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetScalarValueMethods

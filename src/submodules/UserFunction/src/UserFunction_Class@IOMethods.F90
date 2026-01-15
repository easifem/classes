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

SUBMODULE(UserFunction_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
USE BaseType, ONLY: varopt => TypeFEVariableOpt
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: bool1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isInit, "isInit: ", unitNo=unitNo)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display("name: "//obj%name, unitNo=unitNo)
CALL Display(obj%isUserFunctionSet, "isUserFunctionSet: ", unitNo=unitNo)
CALL Display(obj%isLuaScript, "isLuaScript: ", unitNo=unitNo)
IF (obj%isLuaScript) THEN
  CALL Display(obj%luaScript%chars(), "luaScript: ", unitNo=unitNo)
  CALL Display(obj%luaFunctionName%chars(), "luaFunctionName: ", &
               unitNo=unitNo)
END IF

CALL Display(NAME_RETURN_TYPE(obj%returnType), "returnType: ", &
             unitNo=unitNo)
IF (obj%returnType == varopt%matrix) THEN
  CALL Display(obj%returnShape, "shape of returnType: ", unitNo=unitNo)
END IF

CALL Display(NAME_ARG_TYPE(obj%argType), "argType: ", &
             unitNo=unitNo)

CALL Display(obj%numArgs, "number of arguments: ", unitNo=unitNo)
CALL Display(obj%numReturns, "number of returns: ", unitNo=unitNo)

bool1 = ASSOCIATED(obj%scalarFunction)
CALL Display(bool1, msg="scalarFunction ASSOCIATED: ", unitno=unitno)

bool1 = ASSOCIATED(obj%vectorFunction)
CALL Display(bool1, msg="vectorFunction ASSOCIATED: ", unitno=unitno)

bool1 = ASSOCIATED(obj%matrixFunction)
CALL Display(bool1, msg="matrixFunction ASSOCIATED: ", unitno=unitno)

IF (obj%argType == varopt%constant) THEN

  SELECT CASE (obj%returnType)

  CASE (varopt%Scalar)
    CALL Display(obj%scalarValue, "scalarValue: ", unitNo=unitNo)

  CASE (varopt%Vector)
    CALL Display(obj%vectorValue(1:obj%numReturns), "vectorValue: ", &
                 unitNo=unitNo)

  CASE (varopt%Matrix)
    CALL Display(obj%matrixValue(1:obj%returnShape(1), &
                                 1:obj%returnShape(2)), "matrixValue: ", &
                 unitNo=unitNo)

  CASE DEFAULT

  END SELECT
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Vector()"
#endif
#include "../../include/display_vector.F90"
END PROCEDURE obj_Display_Vector

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Ptr_Vector()"
#endif
#include "../../include/display_vector_ptr.F90"
END PROCEDURE obj_Display_Ptr_Vector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods

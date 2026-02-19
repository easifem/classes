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
INTEGER(I4B) :: ii, jj

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

CALL Display("name: "//TRIM(obj%name), unitNo=unitNo)
CALL Display(obj%numArgs, "number of arguments: ", unitNo=unitNo)
CALL Display(obj%numReturns, "number of returns: ", unitNo=unitNo)

CALL Display(NAME_RETURN_TYPE(obj%returnType), "returnType: ", &
             unitNo=unitNo)

IF (obj%returnType == varopt%matrix) THEN
  CALL Display(obj%returnShape, "shape of returnType: ", unitNo=unitNo)
END IF

CALL Display(NAME_ARG_TYPE(obj%argType), "argType: ", unitNo=unitNo)

SELECT CASE (obj%engineID)
CASE (funcopt%externalEngine)
  CALL Display("engineID: externalEngine", unitNo=unitNo)
  CALL Display(obj%isExternalFunc, "isExternalFunc: ", unitNo=unitNo)
  bool1 = ASSOCIATED(obj%scalarFunction)
  CALL Display(bool1, msg="scalarFunction ASSOCIATED: ", unitno=unitno)

  bool1 = ASSOCIATED(obj%vectorFunction)
  CALL Display(bool1, msg="vectorFunction ASSOCIATED: ", unitno=unitno)

  bool1 = ASSOCIATED(obj%matrixFunction)
  CALL Display(bool1, msg="matrixFunction ASSOCIATED: ", unitno=unitno)

CASE (funcopt%luaEngine)
  CALL Display("engineID: luaEngine", unitNo=unitNo)
  CALL Display(obj%isLuaScript, "isLuaScript: ", unitNo=unitNo)

  IF (obj%isLuaScript) THEN
    CALL Display(TRIM(obj%luaScript), "luaScript: ", unitNo=unitNo)
    CALL Display(TRIM(obj%luaFunctionName), "luaFunctionName: ", &
                 unitNo=unitNo)
  END IF

CASE (funcopt%constEngine)
  CALL Display("engineID: constEngine", unitNo=unitNo)

  SELECT CASE (obj%returnType)

  CASE (varopt%scalar)
    CALL Display(obj%scalarValue, "scalarValue: ", unitNo=unitNo)

  CASE (varopt%vector)
    CALL Display(obj%vectorValue(1:obj%numReturns), "vectorValue: ", &
                 unitNo=unitNo)

  CASE (varopt%matrix)
    CALL Display(obj%matrixValue(1:obj%returnShape(1), &
                                 1:obj%returnShape(2)), "matrixValue: ", &
                 unitNo=unitNo)

  CASE DEFAULT
    CALL Display("Unknown returnType", unitNo=unitNo)
  END SELECT

CASE (funcopt%equationParserEngine)
  CALL Display("engineID: equationParserEngine", unitNo=unitNo)

  SELECT CASE (obj%returnType)

  CASE (varopt%scalar)
    CALL obj%scalarEqParser%Display("scalarEqParser: ", unitNo=unitNo)

  CASE (varopt%vector)
    DO ii = 1, obj%numReturns
      bool1 = ASSOCIATED(obj%vectorEqParser(ii)%ptr)
      CALL Display(bool1, "obj%vectorEqParser("//ToString(ii)//")%ptr: ", &
                   unitNo=unitNo)

      IF (bool1) THEN
        CALL obj%vectorEqParser(ii)%ptr%Display( &
          "obj%vectorEqParser("//ToString(ii)//")%ptr: ", unitNo=unitNo)
      END IF
    END DO

  CASE (varopt%matrix)

    DO jj = 1, obj%returnShape(2)
      DO ii = 1, obj%returnShape(1)
        bool1 = ASSOCIATED(obj%matrixEqParser(ii, jj)%ptr)
        CALL Display(bool1, &
                     "obj%matrixEqParser("//ToString(ii)//","// &
                     ToString(jj)//")%ptr ASSOCIATED: ", unitNo=unitNo)

        IF (bool1) THEN
          CALL obj%matrixEqParser(ii, jj)%ptr%Display( &
            "obj%matrixEqParser("//ToString(ii)//","// &
            ToString(jj)//")%ptr: ", unitNo=unitNo)
        END IF
      END DO
    END DO

  CASE DEFAULT
    CALL Display("Unknown returnType", unitNo=unitNo)
  END SELECT

CASE DEFAULT
  CALL Display("no case found for engineID", unitNo=unitNo)
END SELECT

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

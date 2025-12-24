! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(VectorField_Class) TomlMethods
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value

IMPLICIT NONE
CONTAINS

! [kernel.initialCondition]
! name = "initialCondition"
! nodalValueType = "Constant"
! value = 0.0

! [kernel.initialCondition]
! name = "initialCondition"
! nodalValueType = "Space"
! isUserFunction = true
! [kernel.initialCondition.function]
! name = "Function3"
! returnType = "Vector"
! numReturns = 2  # same as spaceCompo
! argType = "Space"
! numArgs = 4
! luaScript = "./functions.lua"
! luaFunctionName = "Function3"

!----------------------------------------------------------------------------
!                                                ImportNodalValueTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportNodalValueTypeFromToml(obj, table, nodalValueType)
  CLASS(VectorField_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: nodalValueType

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportNodalValueTypeFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="nodalValueType", VALUE=astr, &
                default_value=TypeFieldOpt%default_nodalValueType_char, &
                origin=origin, stat=stat)

  nodalValueType = TypeFieldOpt%ToNumber(astr%chars())
  astr = ''

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportNodalValueTypeFromToml

!----------------------------------------------------------------------------
!                                                ImportNodalValueTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportConstantValueFromToml(obj, table)
  CLASS(VectorField_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportConstantValueFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  REAL(DFP) :: VALUE
  REAL(DFP), PARAMETER :: default_constant_value = 0.0_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="value", VALUE=VALUE, origin=origin, stat=stat, &
    default_value=default_constant_value)

  CALL obj%Set(VALUE=VALUE)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportConstantValueFromToml

!----------------------------------------------------------------------------
!                                                   ImportSpaceValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportSpaceValueFromToml(obj, table)
  CLASS(VectorField_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportSpaceValueFromToml()"
#endif

  ! INTEGER(I4B) :: origin, stat
  ! REAL(DFP) :: VALUE
  ! REAL(DFP), PARAMETER :: default_constant_value = 0.0_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportSpaceValueFromToml

!----------------------------------------------------------------------------
!                                                ImportIsUserFunctionFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportIsUserFunctionFromToml(obj, table, isUserFunction)
  CLASS(VectorField_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  LOGICAL(LGT), INTENT(INOUT) :: isUserFunction

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportIsUserFunctionFromToml()"
#endif
  LOGICAL(LGT), PARAMETER :: default_isUserFunction = .FALSE.
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="isUserFunction", VALUE=isUserFunction, &
               default_value=default_isUserFunction, origin=origin, stat=stat)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportIsUserFunctionFromToml

!----------------------------------------------------------------------------
!                                             ImportUserFunctionValueFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportUserFunctionValueFromToml(obj, table)
  CLASS(VectorField_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportUserFunctionValueFromToml()"
  LOGICAL(LGT) :: isok
#endif
  INTEGER(I4B) :: origin, stat
  TYPE(toml_table), POINTER :: node
  TYPE(UserFunction_) :: func

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  node => NULL()
  CALL toml_get(table, "function", node, origin=origin, requested=.FALSE., &
                stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, 'following error occured while reading &
                   &the toml file :: cannot find [function] table in config.')
#endif

  CALL func%ImportFromToml(table=node)
  CALL obj%SetByFunction(func=func)

  CALL func%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportUserFunctionValueFromToml

!----------------------------------------------------------------------------
!                                                           SetFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFromToml
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFromToml()"
#endif

INTEGER(I4B) :: nodalValueType
LOGICAL(LGT) :: isUserFunction

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ImportIsUserFunctionFromToml(obj=obj, table=table, &
                                  isUserFunction=isUserFunction)

IF (isUserFunction) THEN

  CALL ImportUserFunctionValueFromToml(obj=obj, table=table)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

CALL ImportNodalValueTypeFromToml(obj=obj, table=table, &
                                  nodalValueType=nodalValueType)

SELECT CASE (nodalValueType)
CASE (TypeFieldOpt%constant)
  CALL ImportConstantValueFromToml(obj=obj, table=table)
CASE (TypeFieldOpt%space)
  CALL ImportSpaceValueFromToml(obj=obj, table=table)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for nodalValueType')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFromToml

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

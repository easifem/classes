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

SUBMODULE(AbstractMeshField_Class) GetMethods
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Shape()"
#endif

INTEGER(I4B) :: iel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel = obj%mesh%GetLocalElemNumber(globalelement=globalElement, &
                                  islocal=islocal)

SELECT CASE (obj%rank)

CASE (fevaropt%scalar)

  SELECT CASE (obj%vartype)

  CASE (typefield%constant)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (typefield%space, typefield%Time)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (typefield%spaceTime)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
                      "No case found for vartype="//ToString(obj%vartype))
#endif

  END SELECT

CASE (fevaropt%vector)

  SELECT CASE (obj%vartype)

  CASE (typefield%constant)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (typefield%space, typefield%time)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

  CASE (typefield%spaceTime)

    ALLOCATE (ans(3))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
                      "No case found for vartype="//ToString(obj%vartype))
#endif

  END SELECT

CASE (fevaropt%matrix)

  SELECT CASE (obj%vartype)

  CASE (typefield%constant)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

  CASE (typefield%space, typefield%time)

    ALLOCATE (ans(3))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)

  CASE (typefield%spaceTime)

    ALLOCATE (ans(4))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)
    ans(4) = obj%ss(obj%indxShape(iel) + 3)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(.FALSE., myName, &
                      "No case found for vartype="//ToString(obj%vartype))
#endif

  END SELECT

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    "No case found for rank="//ToString(obj%rank))
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get()"
#endif
INTEGER(I4B) :: iel, ii, a, b

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%fieldType .EQ. TypeField%constant) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

a = obj%indxVal(iel)
b = obj%indxVal(iel + 1)

fevar%len = b - a
fevar%capacity = MAX(fevar%len, fevar%capacity)

CALL Reallocate(fevar%val, fevar%capacity)

DO ii = a, b - 1
  fevar%val(ii - a + 1) = obj%val(ii)
END DO

a = obj%indxShape(iel)
b = obj%indxShape(iel + 1) - 1
DO ii = a, b
  fevar%s(ii - a + 1) = obj%ss(ii)
END DO

fevar%defineOn = obj%defineOn
fevar%varType = obj%varType
fevar%rank = obj%rank

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = ""

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

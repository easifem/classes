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

USE GlobalData, ONLY: Constant, Space, Time, SpaceTime, &
                      Scalar, Vector, Matrix

USE Display_Method, ONLY: ToString

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape
INTEGER(I4B) :: iel

iel = obj%mesh%GetLocalElemNumber(globalelement=globalElement, &
                                  islocal=islocal)

SELECT CASE (obj%rank)

CASE (Scalar)

  SELECT CASE (obj%vartype)

  CASE (Constant)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (Space, Time)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (SpaceTime)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

  CASE default

    CALL no_case_found_error

  END SELECT

CASE (Vector)

  SELECT CASE (obj%vartype)

  CASE (Constant)

    ALLOCATE (ans(1))
    ans(1) = obj%ss(obj%indxShape(iel))

  CASE (Space, Time)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

  CASE (SpaceTime)

    ALLOCATE (ans(3))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)

  CASE default

    CALL no_case_found_error

  END SELECT

CASE (Matrix)

  SELECT CASE (obj%vartype)

  CASE (Constant)

    ALLOCATE (ans(2))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)

  CASE (Space, Time)

    ALLOCATE (ans(3))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)

  CASE (SpaceTime)

    ALLOCATE (ans(4))
    ans(1) = obj%ss(obj%indxShape(iel))
    ans(2) = obj%ss(obj%indxShape(iel) + 1)
    ans(3) = obj%ss(obj%indxShape(iel) + 2)
    ans(4) = obj%ss(obj%indxShape(iel) + 3)

  CASE default

    CALL no_case_found_error

  END SELECT

CASE default

  CALL no_case_found_error

END SELECT

CONTAINS

SUBROUTINE no_case_found_error

  CHARACTER(*), PARAMETER :: myName = "obj_Shape()"
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found')

END SUBROUTINE no_case_found_error

END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get
INTEGER(I4B) :: iel, ii, a, b

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

END PROCEDURE obj_Get

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
ans = ""
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

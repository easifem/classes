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
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
CHARACTER(*), PARAMETER :: myName = "obj_Size()"

IF (PRESENT(dim)) THEN
  ans = obj%s(dim)
  RETURN
END IF

SELECT CASE (obj%rank)
CASE (Scalar)
  ans = 1
CASE (Vector)
  ans = obj%s(1)
CASE (Matrix)
  ans = obj%s(1) * obj%s(2)

CASE DEFAULT
  ans = 0
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for obj%rank')
  RETURN
END SELECT
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape

SELECT CASE (obj%rank)
CASE (Scalar)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = [1]
  CASE (Space, Time)
    ans = obj%s(1:1)
  CASE (SpaceTime)
    ans = obj%s(1:2)
  CASE default
    CALL no_case_found_error

  END SELECT

CASE (Vector)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = obj%s(1:1)
  CASE (Space, Time)
    ans = obj%s(1:2)
  CASE (SpaceTime)
    ans = obj%s(1:3)
  CASE default
    CALL no_case_found_error

  END SELECT

CASE (Matrix)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = obj%s(1:2)
  CASE (Space, Time)
    ans = obj%s(1:3)
  CASE (SpaceTime)
    ans = obj%s(1:4)
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
INTEGER(I4B) :: iel, ii

IF (obj%fieldType .EQ. TypeField%constant) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)

END IF

fevar%len = obj%indxVal(iel + 1) - obj%indxVal(iel)
fevar%capacity = MAX(fevar%len, fevar%capacity)

CALL Reallocate(fevar%val, fevar%capacity)

DO ii = obj%indxVal(iel), obj%indxVal(iel + 1) - 1
  fevar%val(ii - obj%indxVal(iel) + 1) = obj%val(ii)
END DO

fevar%s = obj%s

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

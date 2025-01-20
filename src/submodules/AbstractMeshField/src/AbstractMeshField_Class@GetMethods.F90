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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get
CHARACTER(*), PARAMETER :: myName = "obj_Get"
INTEGER(I4B) :: iel
LOGICAL(LGT) :: problem

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  fevar%val = obj%val(:, 1)
  fevar%s = obj%s
  fevar%defineOn = obj%defineOn
  fevar%varType = obj%varType
  fevar%rank = obj%rank
  RETURN
END IF

problem = .NOT. PRESENT(globalElement)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalElement should be present, '//  &
    & 'when mesh field is not constant')
  RETURN
END IF

problem = .NOT. (obj%mesh%IsElementPresent(globalElement))

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: globalElement = '//tostring(globalElement)//  &
    & 'is not present in the mesh.')
  RETURN
END IF

iel = obj%mesh%GetLocalElemNumber(globalElement)
fevar%val = obj%val(:, iel)
fevar%s = obj%s
fevar%defineOn = obj%defineOn
fevar%varType = obj%varType
fevar%rank = obj%rank
END PROCEDURE obj_Get

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
ans = ""
END PROCEDURE obj_GetPrefix

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
  END SELECT
CASE (Vector)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = obj%s(1:1)
  CASE (Space, Time)
    ans = obj%s(1:2)
  CASE (SpaceTime)
    ans = obj%s(1:3)
  END SELECT
CASE (Matrix)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = obj%s(1:2)
  CASE (Space, Time)
    ans = obj%s(1:3)
  CASE (SpaceTime)
    ans = obj%s(1:4)
  END SELECT
END SELECT
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
IF (PRESENT(dim)) THEN
  ans = obj%s(dim)
ELSE
  SELECT CASE (obj%rank)
  CASE (Scalar)
    ans = 1
  CASE (Vector)
    ans = obj%s(1)
  CASE (Matrix)
    ans = obj%s(1) * obj%s(2)
  END SELECT
END IF
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
IF (ALLOCATED(obj%val)) THEN
  ans => obj%val
ELSE
  ans => NULL()
END IF
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods

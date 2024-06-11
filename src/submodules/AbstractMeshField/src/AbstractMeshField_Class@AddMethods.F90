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

SUBMODULE(AbstractMeshField_Class) AddMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 MasterAdd
!----------------------------------------------------------------------------

SUBROUTINE MasterAdd(val, add_val, scale, iel)
  REAL(DFP), INTENT(INOUT) :: val(:, :)
  REAL(DFP), INTENT(IN) :: add_val(:)
  REAL(DFP), INTENT(IN) :: scale
  INTEGER(I4B), INTENT(IN) :: iel

  val(:, iel) = val(:, iel) + scale * add_val(:)
END SUBROUTINE MasterAdd

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add1
INTEGER(I4B) :: iel

IF (obj%fieldType .EQ. TypeField%Constant) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

CALL MasterAdd(obj%val, fevar%val, scale, iel)
END PROCEDURE obj_Add1

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add2
INTEGER(I4B) :: iel, telem

telem = obj%mesh%GetTotalElements()

DO iel = 1, telem
  CALL MasterAdd(obj%val, fevar%val, scale, iel)
END DO

END PROCEDURE obj_Add2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AddMethods

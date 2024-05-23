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

SUBMODULE(AbstractNodeField_Class) SetMethods
USE InputUtility, ONLY: Input

USE AbstractField_Class, ONLY: FIELD_TYPE_CONSTANT

USE RealVector_Method, ONLY: Set, Add

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
INTEGER(I4B) :: ii, tsize1

IF (PRESENT(dof_tPhysicalVars)) obj%dof_tPhysicalVars = dof_tPhysicalVars
IF (PRESENT(dof_storageFMT)) obj%dof_storageFMT = dof_storageFMT
IF (PRESENT(dof_spaceCompo)) obj%dof_spaceCompo = dof_spaceCompo
IF (PRESENT(dof_timeCompo)) obj%dof_timeCompo = dof_timeCompo
IF (PRESENT(dof_tNodes)) obj%dof_tNodes = dof_tNodes
IF (PRESENT(tSize)) obj%tsize = tsize

IF (PRESENT(dof_names_char)) THEN
  IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
  tsize1 = SIZE(dof_names_char)
  ALLOCATE (obj%dof_names_char(tsize1))

  DO ii = 1, tsize1
    obj%dof_names_char(ii) (1:1) = dof_names_char(ii) (1:1)
  END DO
END IF

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingle
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=AddContribution, default=.FALSE.)

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN

  IF (abool) THEN
    CALL Add(obj%realVec, nodenum=1, VALUE=VALUE, scale=areal)
    RETURN
  END IF

  CALL Set(obj%realVec, nodenum=1, VALUE=VALUE)
  RETURN
END IF

IF (abool) THEN
  CALL Add(obj%realVec, nodenum=indx, VALUE=VALUE, scale=areal)
  RETURN
END IF

CALL Set(obj%realVec, nodenum=indx, VALUE=VALUE)

END PROCEDURE obj_SetSingle

!----------------------------------------------------------------------------
!                                                             SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods

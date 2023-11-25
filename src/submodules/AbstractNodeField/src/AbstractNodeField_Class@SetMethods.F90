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
USE BaseMethod
USE HDF5File_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_SetParam
INTEGER(I4B) :: ii

IF (PRESENT(dof_tPhysicalVars)) THEN
  obj%dof_tPhysicalVars = dof_tPhysicalVars
END IF

IF (PRESENT(dof_storageFMT)) THEN
  obj%dof_storageFMT = dof_storageFMT
END IF

IF (PRESENT(dof_spaceCompo)) THEN
  obj%dof_spaceCompo = dof_spaceCompo
END IF

IF (PRESENT(dof_timeCompo)) THEN
  obj%dof_timeCompo = dof_timeCompo
END IF

IF (PRESENT(dof_tNodes)) THEN
  obj%dof_tNodes = dof_tNodes
END IF

IF (PRESENT(dof_names_char)) THEN
  IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
  ALLOCATE (obj%dof_names_char(SIZE(dof_names_char)))
  DO ii = 1, SIZE(dof_names_char)
    obj%dof_names_char(ii) = obj%dof_names_char(ii)
  END DO
END IF

END PROCEDURE anf_SetParam

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_SetSingle
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  IF (PRESENT(addContribution)) THEN
    CALL add(obj%realVec, nodenum=1, VALUE=VALUE, scale=scale)
  ELSE
    CALL set(obj%realVec, nodenum=1, VALUE=VALUE)
  END IF
ELSE
  IF (PRESENT(addContribution)) THEN
    CALL add(obj%realVec, nodenum=indx, VALUE=VALUE, scale=scale)
  ELSE
    CALL set(obj%realVec, nodenum=indx, VALUE=VALUE)
  END IF
END IF
END PROCEDURE anf_SetSingle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
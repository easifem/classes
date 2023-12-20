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
!

SUBMODULE(ScalarField_Class) DBCMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC1
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC1()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: idof, aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

istimes = PRESENT(times)
aint = 0

#ifdef DEBUG_VER
IF (istimes) THEN
  aint = SIZE(times)
  problem = aint .NE. 1
  IF (problem) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[INERNAL ERROR] :: SIZE( times ) is '//  &
      & tostring(aint)//' which is not equal to 1 ')
    RETURN
  END IF
END IF
#endif

CALL dbc%Get(nodalvalue=nodalvalue, nodenum=nodenum, times=times)

IF (istimes) THEN
  aint = SIZE(nodalvalue, 2)
  DO idof = 1, aint
    CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, idof))
  END DO
  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, 1))
IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC2
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC2()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: ibc, tsize, aint, idof
LOGICAL(LGT) :: istimes, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

istimes = PRESENT(times)
aint = 0

#ifdef DEBUG_VER
IF (istimes) THEN
  aint = SIZE(times)
  problem = aint .NE. 1
  IF (problem) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[INERNAL ERROR] :: SIZE( times ) is '//  &
      & tostring(aint)//' which is not equal to 1 ')
    RETURN
  END IF
END IF
#endif

tsize = SIZE(dbc)

IF (istimes) THEN
  DO ibc = 1, tsize
    CALL dbc(ibc)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum,  &
      & times=times)
    aint = SIZE(nodalvalue, 2)
    DO idof = 1, aint
      CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, idof))
    END DO
  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

DO ibc = 1, tsize
  CALL dbc(ibc)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum)
  CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, 1))
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_ApplyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

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

SUBMODULE(VectorField_Class) DBCMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_applyDirichletBC1
CHARACTER(*), PARAMETER :: myName = "obj_applyDirichletBC1()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: aint, spaceCompo

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
    CALL e%RaiseError(modName//'::'//myName//" - "// &
      & '[INERNAL ERROR] :: SIZE( times ) is '//  &
      & tostring(aint)//' which is not equal to 1 ')
    RETURN
  END IF
END IF
#endif

CALL dbc%Get(nodalvalue=nodalvalue, nodenum=nodenum, times=times)
spaceCompo = dbc%GetDOFNo()
CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, 1),  &
  & spaceCompo=spaceCompo)

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_applyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_applyDirichletBC2
CHARACTER(*), PARAMETER :: myName = "obj_applyDirichletBC2()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, tsize, aint, spaceCompo
LOGICAL(LGT) :: istimes, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

istimes = PRESENT(times)
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

DO idof = 1, tsize
  spaceCompo = dbc(idof)%ptr%GetDOFNo()

  CALL dbc(idof)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum,  &
    & times=times)

  CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, 1), &
    & spaceCompo=spaceCompo)
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

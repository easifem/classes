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

SUBMODULE(STScalarField_Class) DBCMethods
USE Display_Method, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 ApplyDBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_applyDirichletBC1
CHARACTER(*), PARAMETER :: myName = "obj_applyDirichletBC1()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, aint
LOGICAL(LGT) :: problem, istimes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

istimes = PRESENT(times)

#ifdef DEBUG_VER

aint = 0
IF (istimes) THEN
  aint = SIZE(times)
  problem = aint .NE. obj%timeCompo
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INERNAL ERROR] :: SIZE( times ) is '// &
                   ToString(aint)//' which is not equal to obj%timeCompo '// &
                      ' which is '//ToString(obj%timeCompo))
    RETURN
  END IF
END IF

#endif

CALL dbc%Get(nodalvalue=nodalvalue, nodenum=nodenum, times=times)

IF (istimes) THEN

  aint = SIZE(nodalvalue, 2)

  DO idof = 1, aint
    CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, idof), &
                 timecompo=idof, islocal=.TRUE.)
  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

  RETURN
END IF

DO idof = 1, obj%timecompo
  CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, 1), &
               timecompo=idof, islocal=.TRUE.)
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_applyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_applyDirichletBC2
CHARACTER(*), PARAMETER :: myName = "obj_applyDirichletBC2()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, ii, aint, tsize
LOGICAL(LGT) :: istimes, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

istimes = PRESENT(times)

#ifdef DEBUG_VER
aint = 0
IF (istimes) THEN
  aint = SIZE(times)
  problem = aint .NE. obj%timeCompo
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INERNAL ERROR] :: SIZE( times ) is '// &
                   ToString(aint)//' which is not equal to obj%timeCompo '// &
                      ' which is '//ToString(obj%timeCompo))
    RETURN
  END IF
END IF
#endif

tsize = SIZE(dbc)

IF (istimes) THEN
  DO ii = 1, tsize
    CALL dbc(ii)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum,  &
      & times=times)
    aint = SIZE(nodalvalue, 2)
    DO idof = 1, aint
      CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, idof), &
                   timecompo=idof, islocal=.TRUE.)
    END DO
  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

DO ii = 1, tsize
  CALL dbc(ii)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum)
  DO idof = 1, obj%timecompo
    CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, 1), &
                 timecompo=idof, islocal=.TRUE.)
  END DO
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

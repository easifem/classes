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

SUBMODULE(BlockNodeField_Class) DBCMethods
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
INTEGER(I4B) :: idof, spacecompo, ttimecompo, ivar0, aint
LOGICAL(LGT) :: case1, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

ivar0 = Input(default=1_I4B, option=ivar)
ttimecompo = obj%dof.timecomponents.ivar0
spacecompo = dbc%GetDOFNo()
CALL dbc%Get(nodalvalue=nodalvalue, nodenum=nodenum, times=times)

aint = SIZE(nodalvalue, 2)
case1 = aint .EQ. 1

IF (case1) THEN
  DO idof = 1, ttimecompo
    CALL obj%Set( &
      & globalNode=nodenum, &
      & VALUE=nodalvalue(:, 1), &
      & ivar=ivar0, &
      & spacecompo=spacecompo, &
      & timecompo=idof)
  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

problem = aint .NE. ttimecompo
IF (problem) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'SIZE( nodalvalue, 2 ) .NE. ttimecompo')
  RETURN
END IF

DO idof = 1, ttimecompo
  CALL obj%Set( &
    & globalNode=nodenum, &
    & VALUE=nodalvalue(:, idof), &
    & ivar=ivar0, &
    & spacecompo=spacecompo, &
    & timecompo=idof)
END DO

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
INTEGER(I4B) :: ibc, idof, spacecompo, ttimecompo, ivar0, tsize, aint
LOGICAL(LGT) :: case1, problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

ivar0 = Input(default=1_I4B, option=ivar)
ttimecompo = obj%dof.timecomponents.ivar0
tsize = SIZE(dbc)

DO ibc = 1, tsize

  CALL dbc(ibc)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum,  &
  & times=times)

  spacecompo = dbc(ibc)%ptr%GetDOFNo()

  aint = SIZE(nodalvalue, 2)

  case1 = aint .EQ. 1

  IF (case1) THEN
    DO idof = 1, ttimecompo
      CALL obj%Set( &
        & globalNode=nodenum, &
        & VALUE=nodalvalue(:, 1), &
        & ivar=ivar0, &
        & spacecompo=spacecompo, &
        & timecompo=idof)
    END DO
  END IF

  IF (.NOT. case1) THEN
    problem = aint .NE. ttimecompo

    IF (problem) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'SIZE( nodalvalue, 2 ) .NE. ttimecompo')
      RETURN
    END IF

    DO idof = 1, ttimecompo
      CALL obj%Set( &
        & globalNode=nodenum, &
        & VALUE=nodalvalue(:, idof), &
        & ivar=ivar0, &
        & spacecompo=spacecompo, &
        & timecompo=idof)
    END DO
  END IF

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

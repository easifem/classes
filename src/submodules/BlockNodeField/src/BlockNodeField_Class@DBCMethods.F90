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
USE InputUtility, ONLY: Input

USE DOF_Method, ONLY: OPERATOR(.timecomponents.)

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC1
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC1()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, spacecompo, ttimecompo, ivar0, aint, nrow, ncol
LOGICAL(LGT) :: case1, problem

ivar0 = Input(default=1_I4B, option=ivar)
ttimecompo = obj%dof.timecomponents.ivar0
spacecompo = dbc%GetDOFNo()
ncol = ttimecompo

nrow = dbc%GetTotalNodeNum(fedof=obj%fedofs(ivar0)%ptr)
ALLOCATE (nodenum(nrow), nodalvalue(nrow, ncol))
CALL dbc%Get(nodalvalue=nodalvalue, nodenum=nodenum, times=times, nrow=nrow, &
             ncol=ncol, fedof=obj%fedofs(ivar0)%ptr)

aint = SIZE(nodalvalue, 2)
case1 = aint .EQ. 1

IF (case1) THEN
  DO idof = 1, ttimecompo
    CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, 1), &
            ivar=ivar0, spacecompo=spacecompo, timecompo=idof, islocal=.TRUE.)
  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

problem = aint .NE. ttimecompo
IF (problem) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
                  '[INTERNAL ERROR] :: SIZE( nodalvalue, 2 ) .NE. ttimecompo')
  RETURN
END IF

DO idof = 1, ttimecompo
  CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, idof), &
            ivar=ivar0, spacecompo=spacecompo, timecompo=idof, islocal=.TRUE.)
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

END PROCEDURE obj_applyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_applyDirichletBC2
CHARACTER(*), PARAMETER :: myName = "obj_applyDirichletBC2()"
LOGICAL(LGT), PARAMETER :: isExpand = .TRUE.
INTEGER(I4B), PARAMETER :: expandFactor = 2

REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: ibc, idof, spacecompo, ttimecompo, ivar0, tsize, aint, nrow, &
                ncol
LOGICAL(LGT) :: case1, problem

ivar0 = Input(default=1_I4B, option=ivar)
ttimecompo = obj%dof.timecomponents.ivar0
tsize = SIZE(dbc)

ncol = ttimecompo

  nrow = dbc(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedofs(ivar0)%ptr)
  CALL Reallocate(nodalvalue, nrow, ncol, isExpand=isExpand, &
                  expandFactor=expandFactor)

  CALL Reallocate(nodenum, nrow, isExpand=isExpand, &
                  expandFactor=expandFactor)
END DO

DO ibc = 1, tsize
  CALL dbc(ibc)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum, &
                        times=times, nrow=nrow, ncol=ncol, &
                        fedof=obj%fedofs(ivar0)%ptr)

  spacecompo = dbc(ibc)%ptr%GetDOFNo()

  case1 = ncol .EQ. 1

  IF (case1) THEN
    DO idof = 1, ttimecompo

      CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, 1), &
            ivar=ivar0, spacecompo=spacecompo, timecompo=idof, islocal=.TRUE.)

    END DO
  END IF

#ifdef DEBUG_VER
  IF (.NOT. case1) THEN
    problem = ncol .NE. ttimecompo
    IF (problem) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
                  '[INTERNAL ERROR] :: SIZE( nodalvalue, 2 ) .NE. ttimecompo')
      RETURN
    END IF
  END IF
#endif

  IF (.NOT. case1) THEN

    DO idof = 1, ttimecompo
    CALL obj%Set(globalNode=nodenum(1:nrow), timecompo=idof, islocal=.TRUE., &
            VALUE=nodalvalue(1:nrow, idof), ivar=ivar0, spacecompo=spacecompo)

    END DO

  END IF

END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

END PROCEDURE obj_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

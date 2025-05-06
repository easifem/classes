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

SUBMODULE(STVectorField_Class) DBCMethods
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_applyDirichletBC1
CHARACTER(*), PARAMETER :: myName = "obj_applyDirichletBC1()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, aint, nrow, ncol
LOGICAL(LGT) :: istimes, problem

istimes = PRESENT(times)

#ifdef DEBUG_VER
IF (istimes) THEN
  aint = SIZE(times)
  problem = aint .NE. obj%timeCompo
  IF (problem) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
                      '[INERNAL ERROR] :: SIZE( times ) is '// &
                   ToString(aint)//' which is not equal to obj%timeCompo '// &
                      ' which is '//ToString(obj%timeCompo))
    RETURN
  END IF
END IF
#endif

nrow = dbc%GetTotalNodenum(fedof=obj%fedof)
ncol = obj%timeCompo
ALLOCATE (nodalvalue(nrow, ncol), nodenum(nrow))

CALL dbc%Get(nodalvalue=nodalvalue, nodenum=nodenum, times=times, nrow=nrow, &
             ncol=ncol, fedof=obj%fedof)

IF (istimes) THEN
  aint = dbc%GetDOFNo()
  DO idof = 1, obj%timeCompo
    CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, idof), &
                 timeCompo=idof, spacecompo=aint, islocal=.TRUE.)
  END DO
  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

DO idof = 1, obj%timecompo
  aint = dbc%GetDOFNo()
  CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, 1), &
               timecompo=idof, spacecompo=aint, islocal=.TRUE.)
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
LOGICAL(LGT), PARAMETER :: isExpand = .TRUE.
INTEGER(I4B), PARAMETER :: expandFactor = 2

REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: ibc, idof, aint, tsize, nrow, ncol
LOGICAL(LGT) :: istimes

istimes = PRESENT(times)

#ifdef DEBUG_VER
IF (istimes) THEN
  aint = SIZE(times)
  IF (aint .NE. obj%timeCompo) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
                      '[INERNAL ERROR] :: SIZE( times ) is '// &
                   ToString(aint)//' which is not equal to obj%timeCompo '// &
                      ' which is '//ToString(obj%timeCompo))
    RETURN
  END IF
END IF
#endif

tsize = SIZE(dbc)
ncol = obj%timeCompo

DO ibc = 1, tsize
  nrow = dbc(ibc)%ptr%GetTotalNodenum(fedof=obj%fedof)
  CALL Reallocate(nodalvalue, nrow, ncol, isExpand=isExpand, &
                  expandFactor=expandFactor)

  CALL Reallocate(nodenum, nrow, isExpand=isExpand, &
                  expandFactor=expandFactor)
END DO

IF (istimes) THEN
  DO ibc = 1, tsize
    CALL dbc(ibc)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum, &
                          times=times, nrow=nrow, ncol=ncol, fedof=obj%fedof)

    aint = dbc(ibc)%ptr%GetDOFNo()

    DO idof = 1, obj%timecompo
      CALL obj%Set(globalNode=nodenum(1:nrow), &
                   VALUE=nodalvalue(1:nrow, idof), &
                   timecompo=idof, spacecompo=aint, &
                   islocal=.TRUE.)
    END DO

  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

DO ibc = 1, tsize
  CALL dbc(ibc)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum, nrow=nrow, &
                        ncol=ncol, fedof=obj%fedof)

  aint = dbc(ibc)%ptr%GetDOFNo()

  DO idof = 1, obj%timecompo
    CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, 1), &
                 timecompo=idof, spacecompo=aint, islocal=.TRUE.)
  END DO
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

END PROCEDURE obj_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

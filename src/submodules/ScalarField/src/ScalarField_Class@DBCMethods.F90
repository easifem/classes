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
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC1()"
LOGICAL(LGT) :: problem
INTEGER(I4B) :: aint
#endif

REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
LOGICAL(LGT) :: istimes
INTEGER(I4B) :: idof, nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

istimes = PRESENT(times)

#ifdef DEBUG_VER
aint = 0
IF (istimes) THEN
  aint = SIZE(times)
  problem = aint .NE. 1
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      '[INERNAL ERROR] :: SIZE( times ) is '// &
                      ToString(aint)//' which is not equal to 1 ')
    RETURN
  END IF
END IF
#endif

ncol = 1
nrow = dbc%GetTotalNodeNum(fedof=obj%fedof)
ALLOCATE (nodenum(nrow), nodalvalue(nrow, ncol))

CALL dbc%Get(nodalvalue=nodalvalue, nodenum=nodenum, times=times, nrow=nrow, &
             ncol=ncol, fedof=obj%fedof)

IF (istimes) THEN
  DO idof = 1, ncol
    CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:ncol, idof), &
                 islocal=.FALSE.)
  END DO
  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, 1), &
             islocal=.TRUE.)

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ApplyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC2()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B), PARAMETER :: expandFactor = 2
LOGICAL(LGT), PARAMETER :: isExpand = .TRUE.

REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: ibc, tsize, idof, nrow, ncol
LOGICAL(LGT) :: istimes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

istimes = PRESENT(times)
tsize = 0

#ifdef DEBUG_VER
IF (istimes) THEN
  tsize = SIZE(times)
  isok = tsize .EQ. 1
  CALL AssertError1(isok, myName, &
                    '[INERNAL ERROR] :: SIZE( times ) is '// &
                    ToString(tsize)//' which is not equal to 1 ')
END IF
#endif

tsize = SIZE(dbc)

ncol = 1
DO ibc = 1, tsize
  nrow = dbc(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedof)
  CALL Reallocate(nodalvalue, nrow, ncol, isExpand=isExpand, &
                  expandFactor=expandFactor)
  CALL Reallocate(nodenum, nrow, isExpand=isExpand, &
                  expandFactor=expandFactor)
END DO

IF (istimes) THEN
  DO ibc = 1, tsize
    CALL dbc(ibc)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum, &
                          times=times, nrow=nrow, ncol=ncol, fedof=obj%fedof)
    DO idof = 1, ncol
    CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, idof), &
                   islocal=.TRUE.)
    END DO
  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

DO ibc = 1, tsize
  CALL dbc(ibc)%ptr%Get(nodalvalue=nodalvalue, nodenum=nodenum, nrow=nrow, &
                        ncol=ncol, fedof=obj%fedof)

  CALL obj%Set(globalNode=nodenum(1:nrow), VALUE=nodalvalue(1:nrow, 1), &
               islocal=.TRUE.)
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ApplyDirichletBC2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE DBCMethods

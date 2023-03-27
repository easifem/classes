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

MODULE PROCEDURE bnField_applyDirichletBC1
CHARACTER(*), PARAMETER :: myName = "bnField_applyDirichletBC1"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, spacecompo, ttimecompo

CALL dbc%get(nodalvalue=nodalvalue, nodenum=nodenum)
ttimecompo = obj%dof.timecomponents.ivar
spacecompo = dbc%getDOFNo()
IF (SIZE(nodalvalue, 2) .EQ. 1) THEN
  DO idof = 1, ttimecompo
    CALL obj%Set( &
      & globalNode=nodenum, &
      & VALUE=nodalvalue(:, 1), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=idof)
  END DO
ELSE
  IF (SIZE(nodalvalue, 2) .NE. ttimecompo) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'SIZE( nodalvalue, 2 ) .NE. ttimecompo')
  DO idof = 1, ttimecompo
    CALL obj%Set( &
      & globalNode=nodenum, &
      & VALUE=nodalvalue(:, idof), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=idof)
  END DO
END IF
IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
END PROCEDURE bnField_applyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_applyDirichletBC2
CHARACTER(*), PARAMETER :: myName = "bnField_applyDirichletBC2"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: ibc, idof, spacecompo, ttimecompo

ttimecompo = obj%dof.timecomponents.ivar
DO ibc = 1, SIZE(dbc)
  CALL dbc(ibc)%ptr%get(nodalvalue=nodalvalue, nodenum=nodenum)
  spacecompo = dbc(ibc)%ptr%getDOFNo()
  IF (SIZE(nodalvalue, 2) .EQ. 1) THEN
    DO idof = 1, ttimecompo
      CALL obj%Set( &
        & globalNode=nodenum, &
        & VALUE=nodalvalue(:, 1), &
        & ivar=ivar, &
        & spacecompo=spacecompo, &
        & timecompo=idof)
    END DO
  ELSE
    IF (SIZE(nodalvalue, 2) .NE. ttimecompo) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'SIZE( nodalvalue, 2 ) .NE. ttimecompo')
    DO idof = 1, ttimecompo
      CALL obj%Set( &
        & globalNode=nodenum, &
        & VALUE=nodalvalue(:, idof), &
        & ivar=ivar, &
        & spacecompo=spacecompo, &
        & timecompo=idof)
    END DO
  END IF
END DO
IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
END PROCEDURE bnField_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

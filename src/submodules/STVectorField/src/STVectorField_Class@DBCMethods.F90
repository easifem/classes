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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_applyDirichletBC1
CHARACTER(LEN=*), PARAMETER :: myName = "stvField_applyDirichletBC1"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof
!!
!! main
!!
CALL dbc%get(nodalvalue=nodalvalue, nodenum=nodenum)
!!
IF (size(nodalvalue, 2) .EQ. 1) THEN
  !!
  DO idof = 1, obj%timecompo
    CALL obj%Set( &
      & globalNode=nodenum, &
      & value=nodalvalue(:, 1), &
      & timecompo=idof, &
      & spacecompo=dbc%getDOFNo())
  END DO
  !!
ELSE
  !!
  !! check
  !!
  IF (SIZE(nodalvalue, 2) .NE. obj%timeCompo) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'SIZE( nodalvalue, 2 ) .NE. obj%timeCompo')
  !!
  DO idof = 1, obj%timecompo
    CALL obj%Set( &
      & globalNode=nodenum, &
      & value=nodalvalue(:, idof), &
      & timecompo=idof, &
      & spacecompo=dbc%getDOFNo())
  END DO
  !!
END IF
!!
IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
!!
END PROCEDURE stvField_applyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_applyDirichletBC2
CHARACTER(LEN=*), PARAMETER :: myName = "stvField_applyDirichletBC2"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: ibc, idof
!!
!! main
!!
DO ibc = 1, SIZE(dbc)
  !!
  CALL dbc(ibc)%ptr%get(nodalvalue=nodalvalue, nodenum=nodenum)
  !!
  IF (SIZE(nodalvalue, 2) .EQ. 1) THEN
    !!
    DO idof = 1, obj%timecompo
      CALL obj%Set( &
        & globalNode=nodenum, &
        & value=nodalvalue(:, 1), &
        & timecompo=idof, &
        & spacecompo=dbc(ibc)%ptr%getDOFNo())
    END DO
    !!
  ELSE
    !!
    !! check
    !!
    IF (SIZE(nodalvalue, 2) .NE. obj%timeCompo) &
      & CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'SIZE( nodalvalue, 2 ) .NE. obj%timeCompo')
    !!
    DO idof = 1, obj%timecompo
      CALL obj%Set( &
        & globalNode=nodenum, &
        & value=nodalvalue(:, idof), &
        & timecompo=idof, &
        & spacecompo=dbc(ibc)%ptr%getDOFNo())
    END DO
    !!
  END IF
  !!
END DO
!!
IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
!!
END PROCEDURE stvField_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

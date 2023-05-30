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

MODULE PROCEDURE vField_applyDirichletBC1
CHARACTER(*), PARAMETER :: myName = "vField_applyDirichletBC1"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
CALL dbc%get( &
  & nodalvalue=nodalvalue, &
  & nodenum=nodenum)
CALL obj%Set( &
  & globalNode=nodenum, &
  & VALUE=nodalvalue(:, 1), &
  & spaceCompo=dbc%getDOFNo())
IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
END PROCEDURE vField_applyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_applyDirichletBC2
CHARACTER(*), PARAMETER :: myName = "vField_applyDirichletBC2"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof
DO idof = 1, SIZE(dbc)
  CALL dbc(idof)%ptr%get( &
    & nodalvalue=nodalvalue, &
    & nodenum=nodenum)
  CALL obj%Set( &
    & globalNode=nodenum, &
    & VALUE=nodalvalue(:, 1), &
    & spaceCompo=dbc(idof)%ptr%getDOFNo())
END DO
IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
END PROCEDURE vField_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

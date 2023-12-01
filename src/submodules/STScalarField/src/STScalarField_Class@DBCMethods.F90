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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_applyDirichletBC1
CHARACTER(*), PARAMETER :: myName = "stsField_applyDirichletBC1()"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, aint

CALL dbc%get(nodalvalue=nodalvalue, nodenum=nodenum, times=times)

IF (PRESENT(times)) THEN
  aint = SIZE(times)
  IF (aint .NE. obj%timeCompo) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[INERNAL ERROR] :: SIZE( times ) is '//  &
      & tostring(aint)//' which is not equal to obj%timeCompo '//  &
      & ' which is '//tostring(obj%timeCompo))
    RETURN
  END IF
  DO idof = 1, obj%timecompo
    CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, idof),  &
        & timecompo=idof)
  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

DO idof = 1, obj%timecompo
  CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, 1), timecompo=idof)
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
END PROCEDURE stsField_applyDirichletBC1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_applyDirichletBC2
CHARACTER(*), PARAMETER :: myName = "stsField_applyDirichletBC2"
REAL(DFP), ALLOCATABLE :: nodalvalue(:, :)
INTEGER(I4B), ALLOCATABLE :: nodenum(:)
INTEGER(I4B) :: idof, ii, aint

IF (PRESENT(times)) THEN
  aint = SIZE(times)
  IF (aint .NE. obj%timeCompo) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & '[INERNAL ERROR] :: SIZE( times ) is '//  &
      & tostring(aint)//' which is not equal to obj%timeCompo '//  &
      & ' which is '//tostring(obj%timeCompo))
    RETURN
  END IF

  DO ii = 1, SIZE(dbc)
    CALL dbc(ii)%ptr%get(nodalvalue=nodalvalue, nodenum=nodenum,  &
      & times=times)

    DO idof = 1, obj%timecompo
      CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, idof),  &
        & timecompo=idof)
    END DO
  END DO

  IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
  IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
  RETURN
END IF

DO ii = 1, SIZE(dbc)
  CALL dbc(ii)%ptr%get(nodalvalue=nodalvalue, nodenum=nodenum)
  DO idof = 1, obj%timecompo
    CALL obj%Set(globalNode=nodenum, VALUE=nodalvalue(:, 1),  &
      & timecompo=idof)
  END DO
END DO

IF (ALLOCATED(nodalvalue)) DEALLOCATE (nodalvalue)
IF (ALLOCATED(nodenum)) DEALLOCATE (nodenum)
END PROCEDURE stsField_applyDirichletBC2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE DBCMethods

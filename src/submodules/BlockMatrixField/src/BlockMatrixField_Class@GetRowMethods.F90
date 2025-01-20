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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(BlockMatrixField_Class) GetRowMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow1
CHARACTER(*), PARAMETER :: myName = "obj_GetRow1"
CALL e%raiseError(modName//'::'//myName//" - "// &
& 'This routine is not callable for BlockMatrixField')
END PROCEDURE obj_GetRow1

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow2
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(VALUE)) &
  & CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & idof=idof, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & idof=idof, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
NULLIFY (realvec)
END PROCEDURE obj_GetRow2

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow3
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(VALUE)) &
  & CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
NULLIFY (realvec)
END PROCEDURE obj_GetRow3

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow4
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(VALUE)) THEN
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
NULLIFY (realvec)
END PROCEDURE obj_GetRow4

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow5
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(VALUE)) THEN
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
NULLIFY (realvec)
END PROCEDURE obj_GetRow5

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow6
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(VALUE)) THEN
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
NULLIFY (realvec)
END PROCEDURE obj_GetRow6

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow7
REAL(DFP), POINTER :: realvec(:)
IF (PRESENT(VALUE)) THEN
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
NULLIFY (realvec)
END PROCEDURE obj_GetRow7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetRowMethods

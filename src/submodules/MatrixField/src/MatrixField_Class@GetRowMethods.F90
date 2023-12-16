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

SUBMODULE(MatrixField_Class) GetRowMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow1
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "obj_GetRow1"

IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
    & idof=idof, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
  ELSE
    CALL GetRow( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & idof=idof, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
  END IF
END IF

IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & idof=idof, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & idof=idof, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
NULLIFY (realvec)
END PROCEDURE obj_GetRow1

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow2
CALL obj%GetRow( &
  & globalNode=globalNode, &
  & idof=(obj%mat%csr%idof.DOFStartIndex.ivar) + idof - 1, &
  & VALUE=VALUE, &
  & nodefieldVal=nodefieldVal, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_GetRow2

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow3

CALL obj%GetRow( &
  & globalNode=globalNode, &
  & idof=GetIDOF( &
          & obj=obj%mat%csr%idof, &
          & ivar=ivar, &
          & spacecompo=spacecompo, &
          & timecompo=timecompo), &
  & VALUE=VALUE, &
  & nodefieldVal=nodefieldVal, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_GetRow3

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow4
CHARACTER(*), PARAMETER :: myName = "obj_GetRow4"
REAL(DFP), POINTER :: realvec(:)

IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF

IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF

NULLIFY (realvec)

END PROCEDURE obj_GetRow4

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow5
CHARACTER(*), PARAMETER :: myName = "obj_GetRow2"
REAL(DFP), POINTER :: realvec(:)

IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF

IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF

NULLIFY (realvec)

END PROCEDURE obj_GetRow5

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow6
CHARACTER(*), PARAMETER :: myName = "obj_GetRow2"
REAL(DFP), POINTER :: realvec(:)

IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF

IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF

NULLIFY (realvec)

END PROCEDURE obj_GetRow6

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow7
CHARACTER(*), PARAMETER :: myName = "obj_GetRow2"
REAL(DFP), POINTER :: realvec(:)

IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF

IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%GetPointer()
    CALL GetRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF

NULLIFY (realvec)

END PROCEDURE obj_GetRow7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetRowMethods

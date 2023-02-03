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
!                                                                     getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getRow1
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "mField_getRow1"
!
IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL getRow( &
    & obj=obj%mat, &
    & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
    & idof=idof, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
  ELSE
    CALL getRow( &
    & obj=obj%mat, &
    & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
    & idof=idof, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & idof=idof, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & idof=idof, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
NULLIFY (realvec)
END PROCEDURE mField_getRow1

!----------------------------------------------------------------------------
!                                                                 getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getRow2
CALL obj%getRow( &
  & globalNode=globalNode, &
  & idof=(obj%mat%csr%idof.DOFStartIndex.ivar) + idof - 1, &
  & VALUE=VALUE, &
  & nodefieldVal=nodefieldVal, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE mField_getRow2

!----------------------------------------------------------------------------
!                                                                 getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getRow3
!
CALL obj%getRow( &
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
!
END PROCEDURE mField_getRow3

!----------------------------------------------------------------------------
!                                                                 getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getRow4
CHARACTER(*), PARAMETER :: myName = "mField_getRow4"
REAL(DFP), POINTER :: realvec(:)
!
IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
!
NULLIFY (realvec)
!
END PROCEDURE mField_getRow4

!----------------------------------------------------------------------------
!                                                                 getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getRow5
CHARACTER(*), PARAMETER :: myName = "mField_getRow2"
REAL(DFP), POINTER :: realvec(:)
!
IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
!
NULLIFY (realvec)
!
END PROCEDURE mField_getRow5

!----------------------------------------------------------------------------
!                                                                 getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getRow6
CHARACTER(*), PARAMETER :: myName = "mField_getRow2"
REAL(DFP), POINTER :: realvec(:)
!
IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
!
NULLIFY (realvec)
!
END PROCEDURE mField_getRow6

!----------------------------------------------------------------------------
!                                                                 getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getRow7
CHARACTER(*), PARAMETER :: myName = "mField_getRow2"
REAL(DFP), POINTER :: realvec(:)
!
IF (PRESENT(VALUE)) THEN
  IF (obj%isRectangle) THEN
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=VALUE, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  IF (obj%isRectangle) THEN
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domains(1)%ptr%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  ELSE
    realvec => nodeFieldVal%getPointer()
    CALL getRow( &
      & obj=obj%mat, &
      & nodenum=obj%domain%getLocalNodeNumber(globalNode), &
      & ivar=ivar, &
      & spacecompo=spacecompo, &
      & timecompo=timecompo, &
      & VALUE=realvec, &
      & scale=scale, &
      & addContribution=addContribution)
  END IF
END IF
!
NULLIFY (realvec)
!
END PROCEDURE mField_getRow7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetRowMethods

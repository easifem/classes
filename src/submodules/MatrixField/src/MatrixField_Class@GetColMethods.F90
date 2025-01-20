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

SUBMODULE(MatrixField_Class) GetColMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColumn1
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "obj_GetColumn1"
!
IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is not implemented for Rectangle matrix')
END IF
!
IF (PRESENT(VALUE)) THEN
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & idof=idof, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  !
  realvec => nodeFieldVal%GetPointer()
  !
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & idof=idof, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
  !
END IF
!
NULLIFY (realvec)
END PROCEDURE obj_GetColumn1

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColumn2
CALL obj%GetColumn( &
  & globalNode=globalNode, &
  & idof=(obj%mat%csr%jdof.DOFStartIndex.ivar) + idof - 1, &
  & VALUE=VALUE, &
  & nodefieldVal=nodefieldVal, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_GetColumn2

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColumn3
CALL obj%GetColumn( &
  & globalNode=globalNode, &
  & idof=GetIDOF(obj=obj%mat%csr%jdof, &
          & ivar=ivar, spacecompo=spacecompo, &
          & timecompo=timecompo), &
  & VALUE=VALUE, &
  & nodefieldVal=nodefieldVal, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_GetColumn3

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColumn4
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "obj_GetColumn4"
!
!
IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is not implemented for Rectangle matrix')
END IF
!
IF (PRESENT(VALUE)) THEN
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
NULLIFY (realvec)
END PROCEDURE obj_GetColumn4

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColumn5
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "obj_GetColumn5"
!
!
IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is not implemented for Rectangle matrix')
END IF
!
IF (PRESENT(VALUE)) THEN
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
NULLIFY (realvec)
END PROCEDURE obj_GetColumn5

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColumn6
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "obj_GetColumn6"
!
!
IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is not implemented for Rectangle matrix')
END IF
!
IF (PRESENT(VALUE)) THEN
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
NULLIFY (realvec)
END PROCEDURE obj_GetColumn6

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColumn7
REAL(DFP), POINTER :: realvec(:)
CHARACTER(*), PARAMETER :: myName = "obj_GetColumn7"
!
!
IF (obj%isRectangle) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is not implemented for Rectangle matrix')
END IF
!
IF (PRESENT(VALUE)) THEN
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
IF (PRESENT(nodeFieldVal)) THEN
  realvec => nodeFieldVal%GetPointer()
  CALL GetColumn( &
    & obj=obj%mat, &
    & nodenum=obj%domain%GetLocalNodeNumber(globalNode), &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=realvec, &
    & scale=scale, &
    & addContribution=addContribution)
END IF
!
NULLIFY (realvec)
END PROCEDURE obj_GetColumn7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetColMethods

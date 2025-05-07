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
USE CSRMatrix_Method, ONLY: GetRow

USE DOF_Method, ONLY: GetIDOF

USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer

USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow1
CHARACTER(*), PARAMETER :: myName = "obj_GetRow1()"
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(VALUE)) THEN

  CALL GetRow(obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=VALUE, &
              scale=scale, addContribution=addContribution)
  RETURN

END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()
RETURN

END PROCEDURE obj_GetRow1

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow2
INTEGER(I4B) :: ii

ii = GetIDOF(obj=obj%mat%csr%idof, ivar=ivar, idof=idof)

CALL obj%GetRow(globalNode=globalNode, islocal=islocal, &
                idof=ii, VALUE=VALUE, nodefieldVal=nodefieldVal, &
                scale=scale, addContribution=addContribution)
END PROCEDURE obj_GetRow2

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow3
INTEGER(I4B) :: ii

ii = GetIDOF(obj=obj%mat%csr%idof, ivar=ivar, spaceCompo=spaceCompo, &
             timeCompo=timeCompo)

CALL obj%GetRow(globalNode=globalNode, islocal=islocal, &
                idof=ii, VALUE=VALUE, nodefieldVal=nodefieldVal, &
                scale=scale, addContribution=addContribution)

END PROCEDURE obj_GetRow3

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow4
CHARACTER(*), PARAMETER :: myName = "obj_GetRow4()"

REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(VALUE)) THEN

  CALL GetRow(obj=obj%mat, nodenum=globalNode, &
              ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
              VALUE=VALUE, scale=scale, addContribution=addContribution)
  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()
RETURN

END PROCEDURE obj_GetRow4

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow5
CHARACTER(*), PARAMETER :: myName = "obj_GetRow5()"

REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(VALUE)) THEN

  CALL GetRow(obj=obj%mat, nodenum=globalNode, &
              ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
              VALUE=VALUE, scale=scale, addContribution=addContribution)
  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()
RETURN

END PROCEDURE obj_GetRow5

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow6
CHARACTER(*), PARAMETER :: myName = "obj_GetRow6()"

REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(VALUE)) THEN

  CALL GetRow(obj=obj%mat, nodenum=globalNode, &
              ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
              VALUE=VALUE, scale=scale, addContribution=addContribution)
  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()
RETURN

END PROCEDURE obj_GetRow6

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow7
CHARACTER(*), PARAMETER :: myName = "obj_GetRow7()"

REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(VALUE)) THEN

  CALL GetRow(obj=obj%mat, nodenum=globalNode, &
              ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
              VALUE=VALUE, scale=scale, addContribution=addContribution)
  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()
RETURN

END PROCEDURE obj_GetRow7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetRowMethods

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

SUBMODULE(MatrixField_Class) SetColMethods
USE Basetype, ONLY: DOF_

USE DOF_Method, ONLY: GetIDOF

USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer

USE Display_Method, ONLY: ToString

USE CSRMatrix_Method, ONLY: SetColumn, GetDOFPointer

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn1
CHARACTER(*), PARAMETER :: myName = "obj_SetColumn1()"
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(scalarVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=scalarVal)
  RETURN

END IF

IF (PRESENT(vecVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=vecVal)
  RETURN

END IF

IF (PRESENT(nodeFieldVal)) THEN

  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")

  ii = SIZE(realvec)

  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=realvec)

  realvec => NULL()
  RETURN
END IF

END PROCEDURE obj_SetColumn1

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn2
INTEGER(I4B) :: indx
TYPE(DOF_), POINTER :: adof
adof => GetDOFPointer(obj%mat, 1)
indx = GetIDOF(obj=adof, ivar=ivar, idof=idof)
CALL obj%SetColumn(globalNode=globalNode, islocal=islocal, idof=indx, &
                scalarVal=scalarVal, vecVal=vecVal, nodeFieldVal=nodeFieldVal)

END PROCEDURE obj_SetColumn2

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn3
INTEGER(I4B) :: indx
TYPE(DOF_), POINTER :: adof
adof => GetDOFPointer(obj%mat, 1)
indx = GetIDOF(obj=adof, ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo)
CALL obj%SetColumn(globalNode=globalNode, islocal=islocal, idof=indx, &
                scalarVal=scalarVal, vecVal=vecVal, nodeFieldVal=nodeFieldVal)

END PROCEDURE obj_SetColumn3

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn4
CHARACTER(*), PARAMETER :: myName = "obj_SetColumn4()"
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(scalarVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=scalarVal)
  RETURN

END IF

IF (PRESENT(vecVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=vecVal)
  RETURN

END IF

IF (PRESENT(nodeFieldVal)) THEN

  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")

  ii = SIZE(realvec)

  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec)

  realvec => NULL()
  RETURN
END IF

END PROCEDURE obj_SetColumn4

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn5
CHARACTER(*), PARAMETER :: myName = "obj_SetColumn5()"
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(scalarVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=scalarVal)
  RETURN

END IF

IF (PRESENT(vecVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=vecVal)
  RETURN

END IF

IF (PRESENT(nodeFieldVal)) THEN

  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")

  ii = SIZE(realvec)

  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec)

  realvec => NULL()
  RETURN
END IF

END PROCEDURE obj_SetColumn5

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn6
CHARACTER(*), PARAMETER :: myName = "obj_SetColumn6()"
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(scalarVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=scalarVal)
  RETURN

END IF

IF (PRESENT(vecVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=vecVal)
  RETURN

END IF

IF (PRESENT(nodeFieldVal)) THEN

  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")

  ii = SIZE(realvec)

  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec)

  realvec => NULL()
  RETURN
END IF

END PROCEDURE obj_SetColumn6

!----------------------------------------------------------------------------
!                                                                    SetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetColumn7
CHARACTER(*), PARAMETER :: myName = "obj_SetColumn7()"
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#include "./localNodeError.F90"

IF (PRESENT(scalarVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=scalarVal)
  RETURN

END IF

IF (PRESENT(vecVal)) THEN

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=vecVal)
  RETURN

END IF

IF (PRESENT(nodeFieldVal)) THEN

  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")

  ii = SIZE(realvec)

  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetColumn(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec)

  realvec => NULL()
  RETURN
END IF

END PROCEDURE obj_SetColumn7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetColMethods

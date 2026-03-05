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

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "MatrixField_Class@GetRowMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                     GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRow1()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(VALUE)
IF (isok) THEN
  CALL GetRow( &
    obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=VALUE, &
    scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")
#endif

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple( &
  VALUE=realvec, istart=1, iend=ii, stride=1, tsize=tsize)

CALL GetRow( &
  obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=realvec, &
  scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple( &
  VALUE=realvec, istart=1, iend=ii, stride=1)

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRow1

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRow2()"
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = GetIDOF(obj=obj%mat%csr%idof, ivar=ivar, idof=idof)

CALL obj%GetRow(globalNode=globalNode, islocal=islocal, &
                idof=ii, VALUE=VALUE, nodefieldVal=nodefieldVal, &
                scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRow2

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRow3()"
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ii = GetIDOF(obj=obj%mat%csr%idof, ivar=ivar, spaceCompo=spaceCompo, &
             timeCompo=timeCompo)

CALL obj%GetRow(globalNode=globalNode, islocal=islocal, &
                idof=ii, VALUE=VALUE, nodefieldVal=nodefieldVal, &
                scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRow3

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRow4()"
#endif

REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(VALUE)
IF (isok) THEN
  CALL GetRow(obj=obj%mat, nodenum=globalNode, &
              ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
              VALUE=VALUE, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")
#endif

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRow4

!----------------------------------------------------------------------------
!                                                                     GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRow5()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(VALUE)
IF (isok) THEN
  CALL GetRow(obj=obj%mat, nodenum=globalNode, &
              ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
              VALUE=VALUE, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")
#endif

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRow5

!----------------------------------------------------------------------------
!                                                                     GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRow6()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(VALUE)
IF (isok) THEN
  CALL GetRow(obj=obj%mat, nodenum=globalNode, &
              ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
              VALUE=VALUE, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")
#endif

ii = SIZE(realvec)

CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRow6

!----------------------------------------------------------------------------
!                                                                 GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRow7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRow7()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(VALUE)
IF (isok) THEN
  CALL GetRow(obj=obj%mat, nodenum=globalNode, &
              ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
              VALUE=VALUE, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, "problem in get pointer to nodeFieldVal")
#endif

ii = SIZE(realvec)
CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1, tsize=tsize)

CALL GetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
            spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec, &
            scale=scale, addContribution=addContribution)

CALL nodeFieldVal%SetMultiple(VALUE=realvec, istart=1, iend=ii, &
                              stride=1)

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRow7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetRowMethods

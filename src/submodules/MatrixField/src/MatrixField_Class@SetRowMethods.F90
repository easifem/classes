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

SUBMODULE(MatrixField_Class) SetRowMethods
USE Basetype, ONLY: DOF_
USE Basetype, ONLY: math => TypeMathOpt
USE DOF_Method, ONLY: GetIDOF
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer
USE Display_Method, ONLY: ToString
USE CSRMatrix_Method, ONLY: SetRow
USE CSRMatrix_Method, ONLY: GetDOFPointer
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "MatrixField_Class@SetRowMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRow1()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(scalarVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=scalarVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(vecVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=vecVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(nodeFieldVal)
IF (isok) THEN
  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")
#endif

  ii = SIZE(realvec)

  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetRow(obj=obj%mat, nodenum=globalNode, idof=idof, VALUE=realvec)

  realvec => NULL()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetRow1

!----------------------------------------------------------------------------
!                                                                 SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRow2()"
#endif
INTEGER(I4B) :: indx
TYPE(DOF_), POINTER :: adof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

adof => GetDOFPointer(obj%mat, 1)
indx = GetIDOF(obj=adof, ivar=ivar, idof=idof)
CALL obj%SetRow(globalNode=globalNode, islocal=islocal, idof=indx, &
                scalarVal=scalarVal, vecVal=vecVal, &
                nodeFieldVal=nodeFieldVal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetRow2

!----------------------------------------------------------------------------
!                                                                 SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRow3()"
#endif
INTEGER(I4B) :: indx
TYPE(DOF_), POINTER :: adof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

adof => GetDOFPointer(obj%mat, 1)
indx = GetIDOF( &
       obj=adof, ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo)

CALL obj%SetRow( &
  globalNode=globalNode, islocal=islocal, idof=indx, &
  scalarVal=scalarVal, vecVal=vecVal, nodeFieldVal=nodeFieldVal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetRow3

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRow4()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(scalarVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=scalarVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(vecVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=vecVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(nodeFieldVal)
IF (isok) THEN
  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")
#endif

  ii = SIZE(realvec)
  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec)
  realvec => NULL()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetRow4

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRow5()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(scalarVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=scalarVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(vecVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=vecVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(nodeFieldVal)
IF (isok) THEN
  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")
#endif

  ii = SIZE(realvec)
  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec)

  realvec => NULL()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetRow5

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRow6()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(scalarVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=scalarVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(vecVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=vecVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(nodeFieldVal)
IF (isok) THEN
  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")
#endif

  ii = SIZE(realvec)

  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec)

  realvec => NULL()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetRow6

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetRow7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetRow7()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

isok = PRESENT(scalarVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=scalarVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(vecVal)
IF (isok) THEN
  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=vecVal)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(nodeFieldVal)
IF (isok) THEN
  realvec => AbstractNodeFieldGetPointer(nodeFieldVal)

#ifdef DEBUG_VER
  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, "Cannot get pointer from nodeFieldVal")
#endif

  ii = SIZE(realvec)

  CALL nodeFieldVal%GetMultiple(VALUE=realvec, istart=1, iend=ii, &
                                stride=1, tsize=tsize)

  CALL SetRow(obj=obj%mat, nodenum=globalNode, ivar=ivar, &
              spaceCompo=spaceCompo, timeCompo=timeCompo, VALUE=realvec)

  realvec => NULL()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetRow7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetRowMethods

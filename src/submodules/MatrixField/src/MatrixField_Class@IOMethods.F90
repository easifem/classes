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

SUBMODULE(MatrixField_Class) IOMethods
USE AbstractMatrixField_Class, ONLY: AbstractMatrixFieldDisplay
USE Display_Method, ONLY: Display
USE CSRMatrix_Method, ONLY: CSRMatrix_SPY => SPY
USE CSRMatrix_Method, ONLY: CSRMatrix_Display => Display

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractMatrixFieldDisplay(obj=obj, msg=msg, unitno=unitno)

CALL Display(obj%isRectangle, "Is Rectangle: ", unitNo=unitNo)
CALL Display(obj%isSubmatInit, "Is Submatrix Init: ", unitNo=unitNo)
CALL Display(obj%tdbcPtrs, "tdbcPtrs: ", unitNo=unitNo)
CALL Display(obj%tsubIndices, "tsubIndices: ", unitNo=unitNo)
isok = ALLOCATED(obj%dbcPtrs)
CALL Display(isok, "dbcPtrs Allocated: ", unitNo=unitNo)
IF (isok) THEN
  CALL Display(obj%dbcPtrs, "dbcPtrs: ", unitNo=unitNo)
END IF

isok = ALLOCATED(obj%subIndices)
CALL Display(isok, "subIndices Allocated: ", unitNo=unitNo)
IF (isok) THEN
  CALL Display(obj%subIndices, "subIndices: ", unitNo=unitNo)
END IF

CALL CSRMatrix_Display(obj%mat, "CSRMatrix_::mat: ", unitNo=unitNo)

IF (obj%isSubmatInit) THEN
  CALL CSRMatrix_Display(obj%submat, "CSRMatrix_::submat: ", unitNo=unitNo)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                      SPY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SPY
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SPY()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL CSRMatrix_SPY(obj=obj%mat, filename=filename, ext=ext)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SPY

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods

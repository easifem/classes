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

SUBMODULE(CSVFile_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setCSVFileProperties
obj%isQuotedStrings = INPUT(option=isQuotedStrings, default=.TRUE.)
obj%isQuotedData = INPUT(option=isQuotedData, default=.FALSE.)
obj%TrueChar = INPUT(option=TrueChar, default="T")
obj%FalseChar = INPUT(option=FalseChar, default="F")
obj%chunk_size = INPUT(option=chunk_size, default=MAX_CHUNK_SIZE)
IF (PRESENT(echostat)) CALL obj%setEchoStat(echostat)
IF (PRESENT(echounit)) CALL obj%setEchoUnit(echounit)
END PROCEDURE obj_setCSVFileProperties

!----------------------------------------------------------------------------
!                                                             setHeaderIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setHeaderIndx
obj%headerIndx = indx
END PROCEDURE obj_setHeaderIndx

!----------------------------------------------------------------------------
!                                                               setSkipRows
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_setSkipRows
obj%skipRows = indx
CALL RemoveDuplicates(obj%skipRows)
END PROCEDURE obj_setSkipRows

END SUBMODULE SetMethods

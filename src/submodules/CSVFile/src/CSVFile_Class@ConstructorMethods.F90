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

SUBMODULE(CSVFile_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_initiate
CALL TxtFileInitiate( &
  obj=obj, &
  filename=filename, &
  unit=unit, &
  status=status, &
  access=access, &
  form=form, &
  position=position, &
  action=action, &
  pad=pad, &
  recl=recl, &
  comment=comment, &
  separator=INPUT(option=separator, default=comma), &
  delimiter=delimiter)
END PROCEDURE txt_initiate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_Deallocate
CALL TxtFileDeallocate(obj, delete)
obj%nrows = 0
obj%ncols = 0
obj%chunk_size = MAX_CHUNK_SIZE
obj%icol = 0
obj%isQuotedStrings = .TRUE.
obj%isQuotedData = .FALSE.
obj%TrueChar = "T"
obj%FalseChar = "F"
obj%headerIndx = 0
IF (ALLOCATED(obj%header)) DEALLOCATE (obj%header)
IF (ALLOCATED(obj%DATA)) DEALLOCATE (obj%DATA)
IF (ALLOCATED(obj%skipRows)) DEALLOCATE (obj%skipRows)
END PROCEDURE txt_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_Final
CALL obj%DEALLOCATE()
END PROCEDURE txt_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods

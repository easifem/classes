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

SUBMODULE(CSVFile_Class) AuxilaryMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               GetDataType
!----------------------------------------------------------------------------

MODULE PROCEDURE GetDataType
  LOGICAL( LGT ) :: abool
  !!
  !! integer
  !!
  abool = astr%is_integer()
  IF(abool) THEN
    dataType = csv_type_integer
    RETURN
  END IF
  !!
  !! real
  !!
  abool = astr%is_real()
  IF(abool) THEN
    dataType = csv_type_real
    RETURN
  END IF
  !!
  !! logical
  !!
  abool = astr%is_logical()
  IF(abool) THEN
    dataType = csv_type_logical
    RETURN
  END IF
  !!
  !! default is string
  !!
  dataType = csv_type_string
  !!
END PROCEDURE GetDataType

!----------------------------------------------------------------------------
!                                                           GetVariableTypes
!----------------------------------------------------------------------------



END SUBMODULE AuxilaryMethods

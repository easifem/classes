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

MODULE TomlUtility
USE GlobalData
USE tomlf, ONLY: toml_table
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "TomlUtility"
PUBLIC :: GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int8_r1(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), ALLOCATABLE, INTENT(OUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int8_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int16_r1(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int16_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int32_r1(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int32_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int64_r1(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int64_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_real32_r1(table, key, VALUE, origin, stat, &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real32_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                          GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue integer vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_real64_r1(table, key, VALUE, origin, stat, &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real64_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values for a matrix

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int8_r2(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int8_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values for a matrix

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int16_r2(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int16_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values for a matrix

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int32_r2(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int32_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values for a matrix

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int64_r2(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int64_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values for a matrix

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_real32_r2(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real32_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values for a matrix

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_real64_r2(table, key, VALUE, origin, stat,  &
    & isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INout) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real64_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                        GetValue@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate table from toml file

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_from_file(table, afile)
    TYPE(toml_table), ALLOCATABLE, INTENT(INOUT) :: table
    TYPE(TxtFile_), INTENT(INOUT) :: afile
  END SUBROUTINE toml_get_from_file
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                         GetValue@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate table from toml file

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_from_filename(table, filename)
    TYPE(toml_table), ALLOCATABLE, INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE toml_get_from_filename
END INTERFACE GetValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TomlUtility

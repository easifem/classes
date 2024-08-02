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
USE tomlf, ONLY: toml_table

USE GlobalData, ONLY: I4B, INT8, INT16, INT32, INT64, REAL32, REAL64, &
                      stdout, stderr, CHAR_LF, LGT

USE TxtFile_Class, ONLY: TxtFile_

USE ExceptionHandler_Class, ONLY: e

USE String_Class, ONLY: String

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "TomlUtility"

PUBLIC :: GetValue
PUBLIC :: GetValue_
PUBLIC :: TomlArrayLength

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-02
! summary:  GetValue of string

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_string(table, key, VALUE, default_value, &
                                    origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    TYPE(String), INTENT(INOUT) :: VALUE
    CHARACTER(*), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_string
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                            GetValue@Methods
!----------------------------------------------------------------------------

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int8(table, key, VALUE, default_value, &
                                  origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), INTENT(INOUT) :: VALUE
    INTEGER(INT8), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int8

  MODULE SUBROUTINE toml_get_int16(table, key, VALUE, default_value, &
                                   origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), INTENT(INOUT) :: VALUE
    INTEGER(INT16), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int16

  MODULE SUBROUTINE toml_get_int32(table, key, VALUE, default_value, &
                                   origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), INTENT(INOUT) :: VALUE
    INTEGER(INT32), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int32

  MODULE SUBROUTINE toml_get_int64(table, key, VALUE, default_value, &
                                   origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), INTENT(INOUT) :: VALUE
    INTEGER(INT64), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int64

  MODULE SUBROUTINE toml_get_real32(table, key, VALUE, default_value, &
                                    origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), INTENT(INOUT) :: VALUE
    REAL(REAL32), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real32

  MODULE SUBROUTINE toml_get_real64(table, key, VALUE, default_value, &
                                    origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), INTENT(INOUT) :: VALUE
    REAL(REAL64), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real64
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int8_r1(table, key, VALUE, origin, stat, &
                                     isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), ALLOCATABLE, INTENT(OUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int8_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

INTERFACE GetValue_
  MODULE SUBROUTINE toml_get_int8_r1_static(table, key, VALUE, tsize, &
                                            origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int8_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int16_r1(table, key, VALUE, origin, stat, &
                                      isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int16_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                         GetValue@Methods
!----------------------------------------------------------------------------

INTERFACE GetValue_
  MODULE SUBROUTINE toml_get_int16_r1_static(table, key, VALUE, &
                                             tsize, origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int16_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int32_r1(table, key, VALUE, origin, stat, &
                                      isFound)
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

INTERFACE GetValue_
  MODULE SUBROUTINE toml_get_int32_r1_static(table, key, VALUE, tsize, &
                                             origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int32_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int64_r1(table, key, VALUE, origin, stat, &
                                      isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int64_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                         GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-02
! summary:  Get Value without allocation

INTERFACE GetValue_
  MODULE SUBROUTINE toml_get_int64_r1_static(table, key, VALUE, tsize, &
                                             origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_int64_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_real32_r1(table, key, VALUE, origin, stat, &
                                       isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real32_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                         GetValue@Methods
!----------------------------------------------------------------------------

INTERFACE GetValue_
  MODULE SUBROUTINE toml_get_real32_r1_static(table, key, VALUE, tsize, &
                                              origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real32_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                          GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue integer vectors

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_real64_r1(table, key, VALUE, origin, stat, &
                                       isFound)
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

INTERFACE GetValue_
  MODULE SUBROUTINE toml_get_real64_r1_static(table, key, VALUE, tsize, &
                                              origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real64_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values for a matrix

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_int8_r2(table, key, VALUE, origin, stat, &
                                     isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
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
  MODULE SUBROUTINE toml_get_int16_r2(table, key, VALUE, origin, stat, &
                                      isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
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
  MODULE SUBROUTINE toml_get_int32_r2(table, key, VALUE, origin, stat, &
                                      isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
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
  MODULE SUBROUTINE toml_get_int64_r2(table, key, VALUE, origin, stat, &
                                      isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
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
  MODULE SUBROUTINE toml_get_real32_r2(table, key, VALUE, origin, stat, &
                                       isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
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
  MODULE SUBROUTINE toml_get_real64_r2(table, key, VALUE, origin, stat, &
                                       isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE toml_get_real64_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                        GetValue@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate table from toml file

INTERFACE
  MODULE SUBROUTINE toml_get_from_file(table, afile)
    TYPE(toml_table), ALLOCATABLE, INTENT(INOUT) :: table
    TYPE(TxtFile_), INTENT(INOUT) :: afile
  END SUBROUTINE toml_get_from_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetValue@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate table from toml file

INTERFACE
  MODULE SUBROUTINE toml_get_from_filename(table, filename)
    TYPE(toml_table), ALLOCATABLE, INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE toml_get_from_filename
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetValue@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate table from toml file

INTERFACE GetValue
  MODULE SUBROUTINE toml_get_from_file_master(table, afile, filename)
    TYPE(toml_table), ALLOCATABLE, INTENT(INOUT) :: table
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
  END SUBROUTINE toml_get_from_file_master
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                        GetValue@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate table from toml file

INTERFACE TomlArrayLength
  MODULE FUNCTION toml_array_length(table, key, origin, stat) RESULT(ans)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(I4B) :: ans
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
  END FUNCTION toml_array_length
END INTERFACE TomlArrayLength

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TomlUtility

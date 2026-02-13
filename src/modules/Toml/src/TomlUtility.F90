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
!> author: Vikas Sharma, Ph. D.
! date: 2026-02-12
! summary: module for working with toml files
!
!# TomlUtility
!
! This module provides utilities for working with TOML files.
! It uses the `tomlf` package.
! This module provides following routines:
! - [GetValue](./GetValue.md): Getting values from a toml table
! - [GetValue_](./GetValue.md): Gettting values from a toml table
! without allocation.

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
!                                                      GetValue@BasicMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate toml table from toml file (This is private method)

INTERFACE
  MODULE SUBROUTINE GetValue_from_file(table, afile)
    TYPE(toml_table), ALLOCATABLE, INTENT(INOUT) :: table
    TYPE(TxtFile_), INTENT(INOUT) :: afile
  END SUBROUTINE GetValue_from_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetValue@BasicMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate toml table from toml file (This is private method)

INTERFACE
  MODULE SUBROUTINE GetValue_from_filename(table, filename)
    TYPE(toml_table), ALLOCATABLE, INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE GetValue_from_filename
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetValue@BasicMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate table from toml file

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_from_file_master(table, afile, filename)
    TYPE(toml_table), ALLOCATABLE, INTENT(INOUT) :: table
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
  END SUBROUTINE GetValue_from_file_master
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                      GetValue@BasicMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Get the length of the toml array

INTERFACE TomlArrayLength
  MODULE FUNCTION ArrayLength(table, key, origin, stat) RESULT(ans)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(I4B) :: ans
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
  END FUNCTION ArrayLength
END INTERFACE TomlArrayLength

!----------------------------------------------------------------------------
!                                                 GetValue@Int8ScalarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-17
! summary:  Get the value of scalar integer
!
!# GetValue
!
! Get the value of scalar integer from toml file.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_test_1.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int8( &
    table, key, VALUE, default_value, origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), INTENT(INOUT) :: VALUE
    INTEGER(INT8), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int8
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                  GetValue@Int8VectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors
!
!# GetValue
!
! Get the value of integer vector from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int8_r1(table, key, VALUE, origin, stat, &
                                     isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Int8_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                            GetValue@Int8StaticVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the value of scalar integer without allocation
!
!# GetValue_
!
! Get the value of integer vector from toml file without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Int8_r1_static(table, key, VALUE, tsize, &
                                            origin, stat, isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Int8_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                  GetValue@Int8MatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values for integer matrix
!
!# GetValue
!
! Get the value of integer matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int8_r2(table, key, VALUE, origin, stat, &
                                     isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int8_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                           GetValue_@Int8StaticMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the values of integer matrix without allocation
!
!# GetValue_
!
! Get the value of integer matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Int8_r2_static( &
    table, key, VALUE, origin, stat, isFound, nrow, ncol)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT8), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int8_r2_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                 GetValue@Int16ScalarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-17
! summary:  Get the value of scalar integer
!
!# GetValue
!
! Get the value of scalar integer from toml file.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_test_1.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int16(table, key, VALUE, default_value, &
                                   origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), INTENT(INOUT) :: VALUE
    INTEGER(INT16), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int16
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                GetValue@Int16VectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors
!
!# GetValue
!
! Get the value of integer vector from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int16_r1(table, key, VALUE, origin, stat, &
                                      isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Int16_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                          GetValue@Int16StaticVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the value of scalar integer without allocation
!
!# GetValue_
!
! Get the value of integer vector from toml file without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Int16_r1_static( &
    table, key, VALUE, tsize, origin, stat, isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Int16_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                GetValue@Int16MatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get value of integer matrix
!
!# GetValue
!
! Get the value of integer matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_5.F90" %}}
!```
INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int16_r2(table, key, VALUE, origin, stat, &
                                      isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), ALLOCATABLE, INTENT(OUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int16_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                          GetValue@Int16StaticMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get the values of integer matrix without allocation
!
!# GetValue
!
! Get the value of integer matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Int16_r2_static(table, key, VALUE, origin, &
                                             stat, isFound, nrow, ncol)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT16), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int16_r2_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                 GetValue@Int32ScalarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-17
! summary:  Get the value of scalar integer
!
!# GetValue
!
! Get the value of scalar integer from toml file.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_test_1.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int32(table, key, VALUE, default_value, &
                                   origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), INTENT(INOUT) :: VALUE
    INTEGER(INT32), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int32
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                GetValue@Int32VectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors
!
!# GetValue
!
! Get the value of integer vector from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int32_r1(table, key, VALUE, origin, stat, &
                                      isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Int32_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                           GetValue@Int32StaticVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the value of scalar integer without allocation
!
!# GetValue_
!
! Get the value of integer vector from toml file without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Int32_r1_static(table, key, VALUE, tsize, &
                                             origin, stat, isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Int32_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                GetValue@Int32MatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values of integer matrix
!
!# GetValue
!
! Get the value of integer matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int32_r2(table, key, VALUE, origin, stat, &
                                      isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int32_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                GetValue_@Int32StaticMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get the values of integer matrix without allocation
!
!# GetValue
!
! Get the value of integer matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Int32_r2_static(table, key, VALUE, origin, &
                                             stat, isFound, nrow, ncol)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT32), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int32_r2_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                GetValue@Int64ScalarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-17
! summary:  Get the value of scalar integer
!
!# GetValue
!
! Get the value of scalar integer from toml file.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_test_1.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int64(table, key, VALUE, default_value, &
                                   origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), INTENT(INOUT) :: VALUE
    INTEGER(INT64), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int64
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                GetValue@Int64VectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue Integer Vectors
!
!# GetValue
!
! Get the value of integer vector from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int64_r1(table, key, VALUE, origin, stat, &
                                      isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Int64_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                         GetValue_@Int64StaticVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the value of scalar integer without allocation
!
!# GetValue_
!
! Get the value of integer vector from toml file without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Int64_r1_static(table, key, VALUE, tsize, &
                                             origin, stat, isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Int64_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                 GetValue@Int64MatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values of integer matrix
!
!# GetValue
!
! Get the value of integer matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Int64_r2(table, key, VALUE, origin, stat, &
                                      isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int64_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                         GetValue_@Int64StaticMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the values of integer matrix without allocation
!
!# GetValue_
!
! Get the value of integer matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Int64_r2_static(table, key, VALUE, origin, &
                                             stat, isFound, nrow, ncol)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    INTEGER(INT64), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Int64_r2_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                               GetValue@Real32ScalarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-17
! summary:  Get the value of scalar real
!
!# GetValue
!
! Get the value of scalar Real from toml file.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_test_1.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Real32(table, key, VALUE, default_value, &
                                    origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), INTENT(INOUT) :: VALUE
    REAL(REAL32), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Real32
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                               GetValue@Real32VectorMehtods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get the real vectors
!
!# GetValue
!
! Get the value of real vector from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Real32_r1(table, key, VALUE, origin, stat, &
                                       isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Real32_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                         GetValue@Real32StaticVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the real vectors without allocation
!
!# GetValue_
!
! Get the value of real vector from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Real32_r1_static( &
    table, key, VALUE, tsize, origin, stat, isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Real32_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                GetValue@Real32MatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values of a real matrix
!
!# GetValue
!
! Get the value of Real matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Real32_r2(table, key, VALUE, origin, stat, &
                                       isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Real32_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                         GetValue_@Real32StaticMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get the values of real matrix without allocation
!
!# GetValue_
!
! Get the value of Real matrix from toml file without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_r2_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Real32_r2_static(table, key, VALUE, origin, &
                                              stat, isFound, nrow, ncol)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL32), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Real32_r2_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                               GetValue@Real64ScalarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-17
! summary:  Get the value of scalar real
!
!# GetValue
!
! Get the value of scalar Real from toml file.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/GetValue_Real_test_1.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Real64(table, key, VALUE, default_value, &
                                    origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), INTENT(INOUT) :: VALUE
    REAL(REAL64), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Real64
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                               GetValue@Real64VectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get the real vectors
!
!# GetValue
!
! Get the value of real vector from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Real64_r1(table, key, VALUE, origin, stat, &
                                       isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Real64_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                         GetValue@Real64StaticVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the real vectors without allocation
!
!# GetValue_
!
! Get the value of real vector from toml file without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r1_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Real64_r1_static( &
    table, key, VALUE, tsize, origin, stat, isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Real64_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                GetValue@Real64MatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values of a real matrix
!
!# GetValue
!
! Get the value of real matrix from toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_test_5.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Real64_r2(table, key, VALUE, origin, stat, &
                                       isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Real64_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                        GetValue_@Real64StaticMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the values of real matrix without allocation
!
!# GetValue_
!
! Get the value of real matrix from toml file without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_2.F90" %}}
!```
!
!## Example 3
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_3.F90" %}}
!```
!
!## Example 4
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_4.F90" %}}
!```
!
!## Example 5
!
!```fortran
!{{% fortran-code file="examples/GetValue_Int_r2_static_test_5.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Real64_r2_static(table, key, VALUE, origin, &
                                              stat, isFound, nrow, ncol)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    REAL(REAL64), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Real64_r2_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                               GetValue@StringScalarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-02
! summary:  GetValue of String
!
!# GetValue
!
! Get value of string scalar from toml file.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_test_1.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_String( &
    table, key, VALUE, default_value, origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table
    CHARACTER(*), INTENT(IN) :: key
    !! key
    TYPE(String), INTENT(INOUT) :: VALUE
    !! value in String
    CHARACTER(*), INTENT(IN) :: default_value
    !! default value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    !! origin, necessary for debugging
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    !! To check the status of getting the value
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    !! If key is found then isFound is set to true
  END SUBROUTINE GetValue_String
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                               GetValue@StringVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  GetValue of vector of Strings
!
!# GetValue
!
! Get a value of string vector from the toml file.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_r1_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_r1_test_2.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_String_r1(table, key, VALUE, origin, stat, &
                                       isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_String_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                          GetValue@StringStaticVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the value of vector of Strings without allocation
!
!# GetValue_
!
! Get the value of string vector without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_r1_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_r1_static_test_2.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_String_r1_static( &
    table, key, VALUE, tsize, origin, stat, isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    TYPE(String), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_String_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                GetValue@StringMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-15
! summary:  Get values of a matrix of Strings
!
!# GetValue
!
! Get values of a matrix of Strings.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_r2_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_r2_test_2.F90" %}}
!```

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_String_r2(table, key, VALUE, origin, stat, &
                                       isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_String_r2
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                        GetValue_@StringStaticMatrixMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the values of matrix of strings without allocation
!
!# GetValue_
!
! Get the values of matrix of strings without allocation.
!
!## Example 1
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_r2_static_test_1.F90" %}}
!```
!
!## Example 2
!
!```fortran
!{{% fortran-code file="examples/GetValue_String_r2_static_test_2.F90" %}}
!```

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_String_r2_static(table, key, VALUE, origin, &
                                              stat, isFound, nrow, ncol)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    TYPE(String), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_String_r2_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                 GetValue@BoolScalarMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-21
! summary:  Get the value of scalar Boolean

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Bool( &
    table, key, VALUE, default_value, origin, stat, isFound)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    LOGICAL(LGT), INTENT(INOUT) :: VALUE
    LOGICAL(LGT), INTENT(IN) :: default_value
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
  END SUBROUTINE GetValue_Bool
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                 GetValue@BoolVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-29
! summary:  GetValue Integer Vectors

INTERFACE GetValue
  MODULE SUBROUTINE GetValue_Bool_r1(table, key, VALUE, origin, stat, &
                                     isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
    !! It is true if the value is a scalar
  END SUBROUTINE GetValue_Bool_r1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                            GetValue@BoolStaticVectorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the value of vector of Bool without allocation

INTERFACE GetValue_
  MODULE SUBROUTINE GetValue_Bool_r1_static(table, key, VALUE, tsize, &
                                            origin, stat, isFound, isScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    CHARACTER(*), INTENT(IN) :: key
    LOGICAL(LGT), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
    LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isScalar
  END SUBROUTINE GetValue_Bool_r1_static
END INTERFACE GetValue_

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-29
! summary:  GetValue Integer Vectors

! INTERFACE
!   MODULE SUBROUTINE GetValue_Bool_r2(table, key, VALUE, origin, stat, &
!                                      isFound)
!     TYPE(toml_table), INTENT(INOUT) :: table
!     CHARACTER(*), INTENT(IN) :: key
!     LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
!     INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
!     INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
!     LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
!   END SUBROUTINE GetValue_Bool_r2
! END INTERFACE

! INTERFACE GetValue
!   MODULE PROCEDURE GetValue_Bool_r2
! END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-15
! summary:  Get the value of vector of Bool without allocation

! INTERFACE
!   MODULE SUBROUTINE GetValue_Bool_r2_static(table, key, VALUE, nrow, ncol, &
!                                             origin, stat, isFound)
!     TYPE(toml_table), INTENT(INOUT) :: table
!     CHARACTER(*), INTENT(IN) :: key
!     LOGICAL(LGT), INTENT(INOUT) :: VALUE(:, :)
!     INTEGER(I4B), INTENT(OUT) :: nrow, ncol
!     INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: origin
!     INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: stat
!     LOGICAL(LGT), OPTIONAL, INTENT(INOUT) :: isFound
!   END SUBROUTINE GetValue_Bool_r2_static
! END INTERFACE

! INTERFACE GetValue_
!   MODULE PROCEDURE GetValue_Bool_r2_static
! END INTERFACE GetValue_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE TomlUtility

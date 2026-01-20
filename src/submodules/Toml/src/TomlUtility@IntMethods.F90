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

SUBMODULE(TomlUtility) IntMethods
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString, Display
USE tomlf, ONLY: toml_error, &
                 toml_load, &
                 toml_parser_config, &
                 toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_context, &
                 toml_terminal, &
                 toml_load, &
                 toml_array, &
                 toml_stat
USE CSVFile_Class, ONLY: CSVFile_
USE String_Class, ONLY: StringReallocate => Reallocate
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int8()"
#endif
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_Int8

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int16()"
#endif
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_Int16

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int32()"
#endif
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_Int32

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int64()"
#endif
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_Int64

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int8_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int8_r1()"
#endif
INTEGER(INT8) :: temp
INTEGER(INT8), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_Int8_r1

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int8_r1_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int8_r1_static()"
#endif
INTEGER(INT8) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_Int8_r1_static

!----------------------------------------------------------------------------
!                                                                    GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int16_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int16_r1()"
#endif
INTEGER(INT16) :: temp
INTEGER(INT16), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_Int16_r1

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int16_r1_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int16_r1_static()"
#endif
INTEGER(INT16) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_Int16_r1_static

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int32_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int32_r1()"
#endif
INTEGER(INT32) :: temp
INTEGER(INT32), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_Int32_r1

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int32_r1_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int32_r1_static()"
#endif
INTEGER(INT32) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_Int32_r1_static

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int64_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int64_r1()"
#endif
INTEGER(INT64) :: temp
INTEGER(INT64), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_Int64_r1

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int64_r1_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int64_r1_static()"
#endif
INTEGER(INT64) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_Int64_r1_static

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int8_r2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int8_r2()"
#endif
INTEGER(INT8) :: temp
INTEGER(INT8), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_Int8_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int8_r2_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int8_r2_static()"
#endif
INTEGER(INT8) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_Int8_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int16_r2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int16_r2()"
#endif
INTEGER(INT16) :: temp
INTEGER(INT16), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_Int16_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int16_r2_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int16_r2_static()"
#endif
INTEGER(INT16) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_Int16_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int32_r2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int32_r2()"
#endif
INTEGER(INT32) :: temp
INTEGER(INT32), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_Int32_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int32_r2_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int32_r2_static()"
#endif
INTEGER(INT32) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_Int32_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int64_r2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int64_r2()"
#endif
INTEGER(INT64) :: temp
INTEGER(INT64), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_Int64_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Int64_r2_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Int64_r2_static()"
#endif
INTEGER(INT64) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_Int64_r2_static

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IntMethods

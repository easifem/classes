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

SUBMODULE(TomlUtility) RealMethods
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
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real32
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real32()"
#endif
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_Real32

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real64
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real64()"
#endif
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_Real64

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real32_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real32_r1()"
#endif
REAL(REAL32) :: temp
REAL(REAL32), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_Real32_r1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real32_r1_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real32_r1_static()"
#endif
REAL(REAL32) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_Real32_r1_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real64_r1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real64_r1()"
#endif
REAL(REAL64) :: temp
REAL(REAL64), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadVector.F90"
END PROCEDURE GetValue_Real64_r1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real64_r1_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real64_r1_static()"
#endif
REAL(REAL64) :: temp
#include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_Real64_r1_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real32_r2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real32_r2()"
#endif
REAL(REAL32) :: temp
REAL(REAL32), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_Real32_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real32_r2_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real32_r2_static()"
#endif
REAL(REAL32) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_Real32_r2_static

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real64_r2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real64_r2()"
#endif
REAL(REAL64) :: temp
REAL(REAL64), ALLOCATABLE :: tempvalvec(:)
#include "./include/ReadMatrix.F90"
END PROCEDURE GetValue_Real64_r2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Real64_r2_static
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Real64_r2_static()"
#endif
REAL(REAL64) :: temp
#include "./include/ReadMatrixStatic.F90"
END PROCEDURE GetValue_Real64_r2_static

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE RealMethods

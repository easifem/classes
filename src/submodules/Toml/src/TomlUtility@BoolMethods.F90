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

SUBMODULE(TomlUtility) BoolMethods
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
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Bool
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_Bool()"
#endif
#include "./include/ReadScalar.F90"
END PROCEDURE GetValue_Bool

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Bool_r1
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "GetValue_Bool_r1()"
! #endif
! LOGICAL(LGT) :: temp
! LOGICAL(LGT), ALLOCATABLE :: tempvalvec(:)
! #include "./include/ReadVector.F90"
END PROCEDURE GetValue_Bool_r1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_Bool_r1_static
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "GetValue_Bool_r1_static()"
! #endif
! LOGICAL(LGT) :: temp
! #include "./include/ReadVectorStatic.F90"
END PROCEDURE GetValue_Bool_r1_static

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BoolMethods

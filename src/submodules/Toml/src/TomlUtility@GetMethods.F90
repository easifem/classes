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

SUBMODULE(TomlUtility) GetMethods
USE String_Class
USE TxtFile_Class
USE BaseMethod
USE tomlf, ONLY:  &
  & toml_error,  &
  & toml_load,  &
  & toml_parser_config,  &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_context,  &
  & toml_terminal,  &
  & toml_load,  &
  & toml_array,  &
  & toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_int8_r1
#include "./include/ReadVector.inc"
END PROCEDURE toml_get_int8_r1

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_int16_r1
#include "./include/ReadVector.inc"
END PROCEDURE toml_get_int16_r1

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_int32_r1
#include "./include/ReadVector.inc"
END PROCEDURE toml_get_int32_r1

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_int64_r1
#include "./include/ReadVector.inc"
END PROCEDURE toml_get_int64_r1

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_real32_r1
#include "./include/ReadVector.inc"
END PROCEDURE toml_get_real32_r1

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_real64_r1
#include "./include/ReadVector.inc"
END PROCEDURE toml_get_real64_r1

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_int8_r2
#include "./include/ReadMatrix.inc"
END PROCEDURE toml_get_int8_r2

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_int16_r2
#include "./include/ReadMatrix.inc"
END PROCEDURE toml_get_int16_r2

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_int32_r2
#include "./include/ReadMatrix.inc"
END PROCEDURE toml_get_int32_r2

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_int64_r2
#include "./include/ReadMatrix.inc"
END PROCEDURE toml_get_int64_r2

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_real32_r2
#include "./include/ReadMatrix.inc"
END PROCEDURE toml_get_real32_r2

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_real64_r2
#include "./include/ReadMatrix.inc"
END PROCEDURE toml_get_real64_r2

!----------------------------------------------------------------------------
!                                                         toml_get_from_file
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_from_file
CHARACTER(*), PARAMETER :: myName = "toml_get_from_file()"
LOGICAL(LGT) :: isNotOpen, isNotRead
LOGICAL(LGT), PARAMETER :: color = .TRUE.
INTEGER(I4B), PARAMETER :: detail = 1
TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

terminal = toml_terminal(color)
isNotOpen = .NOT. afile%IsOpen()
isNotRead = .NOT. afile%IsRead()

IF (isNotRead .OR. isNotOpen) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The file is not open or does not have '//  &
    & 'the access to read!')
END IF

CALL toml_load(table,  &
  & afile%GetUnitNo(),  &
  & context=context,  &
  & config=toml_parser_config(color=terminal, context_detail=detail), &
  & error=error  &
  & )

IF (ALLOCATED(error)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Some error occured while parsing toml file'//  &
    & ' with following message: '//CHAR_LF//error%message)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE toml_get_from_file

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_from_filename
CHARACTER(*), PARAMETER :: myName = "toml_get_from_filename()"
LOGICAL(LGT), PARAMETER :: color = .TRUE.
INTEGER(I4B), PARAMETER :: detail = 1
TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

terminal = toml_terminal(color)
CALL toml_load(table,  &
  & filename,  &
  & context=context,  &
  & config=toml_parser_config(color=terminal, context_detail=detail), &
  & error=error  &
  & )

IF (ALLOCATED(error)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Some error occured while parsing toml file'//  &
    & ' with following message: '//CHAR_LF//error%message)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE toml_get_from_filename

!----------------------------------------------------------------------------
!                                                                GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_from_file_master
CHARACTER(*), PARAMETER :: myName = "toml_get_from_file_master"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (PRESENT(afile)) THEN
  CALL toml_get_from_file(table=table, afile=afile)
ELSEIF (PRESENT(filename)) THEN
  CALL toml_get_from_filename(table=table, filename=filename)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: either filename or afile should be present!')
  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE toml_get_from_file_master

END SUBMODULE GetMethods

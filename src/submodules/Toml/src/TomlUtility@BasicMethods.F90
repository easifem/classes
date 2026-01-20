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

SUBMODULE(TomlUtility) BasicMethods
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
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_from_file
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_from_file()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT), PARAMETER :: color = math%yes
INTEGER(I4B), PARAMETER :: detail = math%one_i
TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

terminal = toml_terminal(color)

#ifdef DEBUG_VER
isok = afile%IsOpen()
CALL AssertError1(isok, myName, &
                  "File is not open.")
#endif

#ifdef DEBUG_VER
isok = afile%IsRead()
CALL AssertError1(isok, myName, &
                  "File has no read access")
#endif

CALL toml_load( &
  table, afile%GetUnitNo(), context=context, error=error, &
  config=toml_parser_config(color=terminal, context_detail=detail))

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(error)
IF (.NOT. isok) THEN
  CALL AssertError1( &
    isok, myName, &
  'Some error occured while parsing toml file with following message: '//error%message)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_from_file

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_from_filename
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_from_filename()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT), PARAMETER :: color = math%yes
INTEGER(I4B), PARAMETER :: detail = math%one_i
TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

terminal = toml_terminal(color)
CALL toml_load( &
  table, filename, context=context, error=error, &
  config=toml_parser_config(color=terminal, context_detail=detail))

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(error)
IF (.NOT. isok) THEN
  CALL AssertError1( &
    isok, myName, &
  'Some error occured while parsing toml file with following message: '//error%message)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_from_filename

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE GetValue_from_file_master
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "GetValue_from_file_master"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(afile)
IF (isok) CALL GetValue_from_file(table=table, afile=afile)

isok = PRESENT(filename)
IF (isok) CALL GetValue_from_filename(table=table, filename=filename)

#ifdef DEBUG_VER
isok = PRESENT(afile) .OR. PRESENT(filename)
CALL AssertError1(isok, myName, &
                  'either filename or afile should be present!')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE GetValue_from_file_master

!----------------------------------------------------------------------------
!                                                            TomlArrayLength
!----------------------------------------------------------------------------

MODULE PROCEDURE ArrayLength
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ArrayLength()"
#endif

TYPE(toml_array), POINTER :: array
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! try to read from the array
array => NULL()
CALL toml_get(table, key, array, origin=origin, stat=stat, &
              requested=math%no)

ans = 0
isok = ASSOCIATED(array)
IF (isok) ans = toml_len(array)

array => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ArrayLength

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BasicMethods


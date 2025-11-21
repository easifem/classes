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

SUBMODULE(AbstractOneDimFE_Class) TomlMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
CALL obj%opt%ImportFromToml(table=table)
obj%isInit = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

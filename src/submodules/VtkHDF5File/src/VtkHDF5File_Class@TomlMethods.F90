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

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Constructor methods of VtkHDF5File_

SUBMODULE(VtkHDF5File_Class) TomlMethods
USE Display_Method, ONLY: Display
USE ExceptionHandler_Class, ONLY: e
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value
IMPLICIT NONE

#define _VTK_FILENAME_ "./result.vtkhdf"

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "VtkHDF5File_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml()"
#endif
  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%DEALLOCATE()

  node => NULL()
  CALL toml_get(table, "vtkhdf", node, origin=origin, requested=math%no, &
                stat=stat)

  IF (.NOT. ASSOCIATED(node)) THEN
    obj%isRequested = math%no
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] vtkhdf table not found.')
#endif
    RETURN
  END IF

  CALL GetValue(table=node, key="isActive", VALUE=obj%isRequested, &
                default_value=math%yes, origin=origin, stat=stat)

  CALL GetValue(table=node, key="filename", VALUE=obj%outputFile, &
                default_value=_VTK_FILENAME_, origin=origin, stat=stat)

  CALL GetValue(table=node, key="isTransient", VALUE=obj%isTransient, &
                default_value=math%yes, origin=origin, stat=stat)

  CALL GetValue(table=node, key="frequency", VALUE=obj%frequency, &
                default_value=math%one_i, origin=origin, stat=stat)

  obj%frequency = MAX(obj%frequency, 1)

  CALL GetValue(table=node, key="zlib", VALUE=obj%zlib, &
                default_value=-1_I4B, origin=origin, stat=stat)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ImportFromToml

!----------------------------------------------------------------------------
!                                                            Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

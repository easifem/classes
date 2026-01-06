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

SUBMODULE(ConstDirichletBC_Class) TomlMethods
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_len => len
USE tomlf, ONLY: toml_array
USE TomlUtility, ONLY: GetValue
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportConstBCFromToml
CALL obj%ImportConstBCFromToml(table=table, dom=dom)
END PROCEDURE obj_ImportConstBCFromToml

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif
#include "../../include/AbstractBC/ImportFromToml1.F90"
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

#define _IMPORT_FROM_TOML_ ConstDirichletBCImportFromToml
#include "../../include/AbstractBC/ImportFromToml2.F90"
#undef _IMPORT_FROM_TOML_
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

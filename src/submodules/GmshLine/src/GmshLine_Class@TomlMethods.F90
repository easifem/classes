! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(GmshLine_Class) TomlMethods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE tomlf, ONLY: toml_array
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_len => len
USE tomlf, ONLY: toml_serialize
USE TomlUtility, ONLY: GetValue
USE TomlUtility, ONLY: GetValue_
USE BaseType, ONLY: math => TypeMathOpt
USE ReallocateUtility, ONLY: Reallocate
USE StringUtility, ONLY: Uppercase
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "GmshLine_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: indx, pointId(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ImportPointIdFromToml(table=table, ans=pointId)
CALL ImportIndxFromToml(table=table, ans=indx)

CALL obj%Initiate(pointId=pointId, indx=indx)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportPointIdFromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: ans(2)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportPointIdFromToml()"
#endif
  INTEGER(I4B) :: origin, stat, tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading point from toml...')
#endif

  CALL GetValue_(table=table, key="point", VALUE=ans, &
                 origin=origin, stat=stat, isfound=isok, &
                 tsize=tsize)

#ifdef DEBUG_VER
  CALL AssertError2(tsize, math%two_i, myName, &
                    "a=size of points from toml table, b=2")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    "point not found in toml table.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportPointIdFromToml

!----------------------------------------------------------------------------
!                                                         ImportIndxFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportIndxFromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportIndxFromToml()"
#endif
  INTEGER(I4B), PARAMETER :: default_value = math%zero_i
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading id from toml...')
#endif

  CALL GetValue(table=table, key="id", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok, &
                default_value=default_value)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportIndxFromToml

!----------------------------------------------------------------------------
!                                                            ImportFromToml2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

#ifdef DEBUG_VER
isok = ALLOCATED(table)
CALL AssertError1(isok, myName, "table is not allocated from GetValue")
#endif

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=math%no, &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")
#endif

CALL obj%ImportFromToml(table=node)

node => NULL()
DEALLOCATE (table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml3()"
#endif

#define _POLYMORPHIC_TYPE_ GmshLine_
#include "../../include/vector_ptr_importfromtoml_1.F90"
#undef _POLYMORPHIC_TYPE_
END PROCEDURE obj_ImportFromToml3

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml4()"
#endif

#define _IMPORT_FROM_TOML_ GmshLineImportFromToml
#include "../../include/vector_ptr_importfromtoml_2.F90"
#undef _IMPORT_FROM_TOML_
END PROCEDURE obj_ImportFromToml4

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

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

SUBMODULE(GmshPhysicalGroup_Class) TomlMethods
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
CHARACTER(*), PARAMETER :: modName = "GmshPhysicalGroup_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: indx, dim
TYPE(String) :: name
INTEGER(I4B), ALLOCATABLE :: tags(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ImportDimFromToml(table=table, ans=dim)
CALL ImportIndxFromToml(table=table, ans=indx)
CALL ImportNameFromToml(table=table, ans=name, dim=dim, indx=indx)
CALL ImportTagsFromToml(table=table, ans=tags)
CALL obj%Initiate(dim=dim, tags=tags, name=name%Chars(), indx=indx)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                         ImportDimFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportDimFromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportDimFromToml()"
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
                    'Reading dim from toml...')
#endif

  CALL GetValue(table=table, key="dim", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok, &
                default_value=default_value)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, "dim is not found.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportDimFromToml

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
  CALL AssertError1(isok, myName, "id not found.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportIndxFromToml

!----------------------------------------------------------------------------
!                                                         ImportNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportNameFromToml(table, ans, dim, indx)
  CLASS(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: ans
  INTEGER(I4B), INTENT(IN) :: dim, indx

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportNameFromToml()"
#endif
  CHARACTER(:), ALLOCATABLE :: default_value
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading name from toml...')
#endif

  default_value = "physicalEntities_dim_"//ToString(dim)// &
                  "_indx_"//ToString(indx)

  CALL GetValue(table=table, key="name", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok, &
                default_value=default_value)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportNameFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportTagsFromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: ans(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportTagsFromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading tags from toml...')
#endif

  CALL GetValue(table=table, key="tags", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, "tags not found.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportTagsFromToml

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

#define _POLYMORPHIC_TYPE_ GmshPhysicalGroup_
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

#define _IMPORT_FROM_TOML_ GmshPhysicalGroupImportFromToml
#include "../../include/vector_ptr_importfromtoml_2.F90"
#undef _IMPORT_FROM_TOML_
END PROCEDURE obj_ImportFromToml4

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

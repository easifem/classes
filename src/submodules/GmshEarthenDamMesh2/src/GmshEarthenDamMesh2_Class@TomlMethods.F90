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

SUBMODULE(GmshEarthenDamMesh2_Class) TomlMethods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE tomlf, ONLY: toml_array
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_len => len
USE tomlf, ONLY: toml_serialize
USE TomlUtility, ONLY: GetValue
USE TomlUtility, ONLY: GetValue_
USE ReallocateUtility, ONLY: Reallocate
USE StringUtility, ONLY: Uppercase
USE GmshPoint_Class, ONLY: GmshPointImportFromToml
USE GmshLine_Class, ONLY: GmshLineImportFromToml
USE GmshPlaneSurface_Class, ONLY: GmshPlaneSurfaceImportFromToml
USE GmshPhysicalGroup_Class, ONLY: GmshPhysicalGroupImportFromToml
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "GmshEarthenDamMesh2_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml
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

CALL ImportRecombineAllFromToml(table=table, ans=obj%recombineAll)
CALL ImportFileNameFromToml(table=table, ans=obj%filename)
CALL GmshPointImportFromToml(obj=obj%points, table=table, &
                             tomlName="point")

CALL GmshLineImportFromToml(obj=obj%lines, table=table, &
                            tomlName="line")

CALL GmshPlaneSurfaceImportFromToml(obj=obj%surfaces, table=table, &
                                    tomlName="planeSurface")

CALL GmshPhysicalGroupImportFromToml(obj=obj%physicalGroups, table=table, &
                                     tomlName="physicalGroup")

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportRecombineAllFromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  LOGICAL(LGT), INTENT(OUT) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportRecombineAllFromToml()"
#endif
  LOGICAL(LGT), PARAMETER :: default_value = math%no
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading recombineAll from toml...')
#endif

  CALL GetValue(table=table, key="recombineAll", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok, &
                default_value=default_value)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportRecombineAllFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ImportFileNameFromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportFileNameFromToml()"
#endif
  CHARACTER(*), PARAMETER :: default_value = "EarthenDamMesh2.msh"
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading filename from toml...')
#endif

  CALL GetValue(table=table, key="filename", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok, &
                default_value=default_value)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportFileNameFromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml
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
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

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

SUBMODULE(SolidMaterial_Class) TomlMethods
USE MeshSelection_Class, ONLY: MeshSelectionReallocate
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: ToString, Display

USE MaterialFactory, ONLY: SolidMechanicsModelFactory, &
                           SolidMaterialFactory
USE TomlUtility, ONLY: GetValue

USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_array, &
                 toml_stat
USE AbstractMaterial_Class, ONLY: AbstractMaterialImportFromToml

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
TYPE(String) :: astr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL AbstractMaterialImportFromToml(obj=obj, table=table, region=region, &
                                    dom=dom)

CALL GetValue(table=table, key="stressStrainModel", &
              VALUE=astr, default_value="NONE", origin=origin, &
              stat=stat, isFound=isok)

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  node => NULL()
  astr = ""
  RETURN
END IF

node => NULL()
CALL toml_get(table, astr%chars(), node, &
              origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "stressStrainModel"//'='//astr//', but table '// &
                  astr//' not found.')
#endif

obj%stressStrainModel => SolidMechanicsModelFactory(astr%chars())

CALL obj%stressStrainModel%ImportFromToml(table=node)

node => NULL()
astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

LOGICAL(LGT) :: isok
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat, ii, mysize
LOGICAL(LGT) :: isRegion

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isRegion = PRESENT(region)
mysize = SIZE(materialNames)

tsize = 0
DO ii = 1, mysize
  node => NULL()
  CALL toml_get(table, materialNames(ii)%chars(), node, origin=origin, &
                requested=.FALSE., stat=stat)

  isok = ASSOCIATED(node)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
               'Cannot find/read materialName='//materialNames(ii)%chars()// &
                    ' in the config file.')
#endif

  IF (isok) tsize = tsize + 1

  isok = .NOT. ASSOCIATED(obj(ii)%ptr)
  IF (isok) ALLOCATE (obj(ii)%ptr)

  IF (isRegion) THEN
    isok = .NOT. ASSOCIATED(region(ii)%ptr)
    IF (isok) ALLOCATE (region(ii)%ptr)
    CALL obj(ii)%ptr%ImportFromToml(table=node, region=region(ii)%ptr, &
                                    dom=dom)
  ELSE
    CALL obj(ii)%ptr%ImportFromToml(table=node)
  END IF

END DO

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                       ReadPropNamesFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadMaterialNamesFromToml(table, materialNames, tsize)
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), ALLOCATABLE, INTENT(INOUT) :: materialNames(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadMaterialNamesFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, ii
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading solidMaterialNames ...')
#endif

  isok = ALLOCATED(materialNames)
  IF (isok) THEN
    tsize = SIZE(materialNames)
    DO ii = 1, tsize
      materialNames(ii) = ""
    END DO
    DEALLOCATE (materialNames)
  END IF

  CALL GetValue(table=table, key="solidMaterialNames", VALUE=materialNames, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'Cannot find/read "materialNames" in the config file.')
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(materialNames)
  CALL AssertError1(isok, myName, &
                   'Could not allocate "materialNames" from the config file.')
#endif

  tsize = SIZE(materialNames)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadMaterialNamesFromToml

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml3()"
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok
TYPE(String), ALLOCATABLE :: materialNames(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
      'following error occured while reading the toml file :: cannot find [' &
                  //tomlName//"] table in config.")
#endif

CALL ReadMaterialNamesFromToml(table=node, materialNames=materialNames, &
                               tsize=tsize)
CALL SolidMaterialReallocate(obj, tsize)

isok = PRESENT(region)
IF (isok) THEN
  CALL MeshSelectionReallocate(region, tsize)
END IF
CALL SolidMaterialImportFromToml(obj=obj, table=node, &
                                 materialNames=materialNames, &
                                 tsize=tsize, region=region, dom=dom)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml3

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

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
LOGICAL(LGT) :: isok
#endif

TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
TYPE(String) :: astr
LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractMaterialImportFromToml(obj=obj, table=table)

CALL GetValue(table=table, key="stressStrainModel", &
              VALUE=astr, default_value="NONE", origin=origin, &
              stat=stat, isFound=isFound)

IF (.NOT. isFound) THEN
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

SUBROUTINE obj_ImportFromToml2_part1(obj, tsize, table, tomlName)
  TYPE(SolidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
  INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of the array of SolidMaterialPointer_
    !! Allocated inside the routine
  TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
  CHARACTER(*), INTENT(IN) :: tomlName

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2_part1()"
#endif

  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
                stat=stat)

  isok = ASSOCIATED(node)

  IF (.NOT. isok) THEN
    tsize = 0
    node => NULL()
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  tsize = 1
  CALL SolidMaterialReallocate(obj, tsize)

  isok = .NOT. ASSOCIATED(obj(1)%ptr)
  IF (isok) ALLOCATE (obj(1)%ptr)

  CALL obj(1)%ptr%ImportFromToml(table=node)
  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE obj_ImportFromToml2_part1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_ImportFromToml2_part2(obj, tsize, table, tomlName)
  TYPE(SolidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
  INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of the array of SolidMaterialPointer_
    !! Allocated inside the routine
  TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
  CHARACTER(*), INTENT(IN) :: tomlName
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2_part2()"
#endif

  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  TYPE(toml_array), POINTER :: array
  INTEGER(I4B) :: origin, stat, ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  array => NULL()
  CALL toml_get(table, tomlName, array, origin=origin, &
                requested=.FALSE., stat=stat)

  isok = ASSOCIATED(array)
  IF (isok) THEN
    tsize = toml_len(array)
  ELSE
    tsize = 0
  END IF

  CALL SolidMaterialReallocate(obj, tsize)

  DO ii = 1, tsize
    node => NULL()
    CALL toml_get(array, ii, node)

#ifdef DEBUG_VER
    isok = ASSOCIATED(node)
    CALL AssertError1(isok, myName, &
          tomlName//'['//tostring(ii)//'] cannot be read from the toml file.')
#endif

    isok = .NOT. ASSOCIATED(obj(ii)%ptr)
    IF (isok) ALLOCATE (obj(ii)%ptr)

    CALL obj(ii)%ptr%ImportFromToml(table=node)
  END DO

  array => NULL()
  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE obj_ImportFromToml2_part2

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj_ImportFromToml2_part1(obj=obj, tsize=tsize, table=table, &
                               tomlName=tomlName)

CALL obj_ImportFromToml2_part2(obj=obj, tsize=tsize, table=table, &
                               tomlName=tomlName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml3()"
#endif

TYPE(toml_table), ALLOCATABLE :: table

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

CALL SolidMaterialImportFromToml(obj=obj, tsize=tsize, &
                                 table=table, tomlName=tomlName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml3

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE obj_ImportFromToml4_part1(obj, tsize, table, tomlName, &
                                     regions, dom)
  TYPE(SolidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  !! Should be allocated outside
  INTEGER(I4B), INTENT(OUT) :: tsize
  !! Size of the array of SolidMaterialPointer_
  TYPE(toml_table), INTENT(INOUT) :: table
  !! Toml table to returned
  CHARACTER(*), INTENT(IN) :: tomlName
  TYPE(MeshSelectionPointer_), ALLOCATABLE, INTENT(INOUT) :: regions(:)
  !! Mesh regions which are contained in the solid materials
  CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml4_part1()"
#endif

  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node, regionNode
  INTEGER(I4B) :: origin, stat
  TYPE(String) :: materialName
  TYPE(MeshSelection_), POINTER :: region

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

  CALL toml_get(table, tomlName, node, origin=origin, &
                requested=.FALSE., stat=stat)

  isok = ASSOCIATED(node)

  IF (.NOT. isok) THEN
    tsize = 0
    node => NULL()

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  tsize = 1
  CALL SolidMaterialReallocate(obj, tsize)
  CALL MeshSelectionReallocate(regions, tsize)

  CALL GetValue(table=node, key="materialName", VALUE=materialName, &
                default_value="NA", origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    tomlName//'[1] materialName field is missing.')
#endif

#ifdef DEBUG_VER
  isok = .NOT. ASSOCIATED(obj(1)%ptr)
  CALL AssertError1(isok, myName, &
                    'obj(1)%ptr is already associated.')
#endif

  obj(1)%ptr => SolidMaterialFactory(materialName%chars())
  !! info: Solid material factory is defined in MaterialFactory.

  CALL obj(1)%ptr%ImportFromToml(table=node)

  CALL toml_get(node, "region", regionNode, &
                origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(regionNode)
  CALL AssertError1(isok, myName, &
                    tomlName//'[1] region field is missing.')
#endif

  ALLOCATE (region)
  CALL region%ImportFromToml(table=regionNode, dom=dom)

#ifdef DEBUG_VER
  isok = .NOT. ASSOCIATED(regions(1)%ptr)
  CALL AssertError1(isok, myName, &
                    'regions(1)%ptr is already associated.')
#endif

  regions(1)%ptr => region

  region => NULL()
  node => NULL()
  regionNode => NULL()
  materialName = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END]')
#endif
END SUBROUTINE obj_ImportFromToml4_part1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE obj_ImportFromToml4_part2(obj, tsize, table, tomlName, &
                                     regions, dom)
  TYPE(SolidMaterialPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
  INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of the array of SolidMaterialPointer_
  TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
  CHARACTER(*), INTENT(IN) :: tomlName
  TYPE(MeshSelectionPointer_), ALLOCATABLE, INTENT(INOUT) :: regions(:)
  CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml4_part2()"
#endif

  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node, regionNode
  TYPE(toml_array), POINTER :: array
  INTEGER(I4B) :: origin, stat, ii
  TYPE(String) :: materialName
  TYPE(MeshSelection_), POINTER :: region

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

  array => NULL()
  CALL toml_get(table, tomlName, array, origin=origin, &
                requested=.FALSE., stat=stat)

  isok = ASSOCIATED(array)
  tsize = 0; IF (isok) tsize = toml_len(array)

  CALL SolidMaterialReallocate(obj, tsize)
  CALL MeshSelectionReallocate(regions, tsize)

  DO ii = 1, tsize
    node => NULL()
    CALL toml_get(array, ii, node)

#ifdef DEBUG_VER
    isok = ASSOCIATED(node)
    CALL AssertError1(isok, myName, &
          tomlName//'['//ToString(ii)//'] cannot be read from the toml file.')
#endif

    CALL GetValue(table=node, key="materialName", VALUE=materialName, &
                  default_value="NA", origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
    CALL AssertError1(isok, myName, &
              tomlName//'['//ToString(ii)//"] materialName field is missing.")
#endif

#ifdef DEBUG_VER
    isok = .NOT. ASSOCIATED(obj(ii)%ptr)
    CALL AssertError1(isok, myName, &
                      'obj('//ToString(ii)//')%ptr is already associated.')
#endif

    obj(ii)%ptr => SolidMaterialFactory(materialName%chars())
    !! info: Solid material factory is defined in MaterialFactory.

    CALL obj(ii)%ptr%ImportFromToml(table=node)

    CALL toml_get(node, "region", regionNode, &
                  origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
    isok = ASSOCIATED(regionNode)
    CALL AssertError1(isok, myName, &
                    tomlName//'['//ToString(ii)//'] region field is missing.')
#endif

    ALLOCATE (region)
    CALL region%ImportFromToml(table=regionNode, dom=dom)

#ifdef DEBUG_VER
    isok = .NOT. ASSOCIATED(regions(ii)%ptr)
    CALL AssertError1(isok, myName, &
                     'regions('//ToString(ii)//')%ptr is already associated.')
#endif

    ALLOCATE (regions(ii)%ptr)
    regions(ii)%ptr => region

  END DO

  region => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END]')
#endif
END SUBROUTINE obj_ImportFromToml4_part2

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml4()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj_ImportFromToml4_part1(obj=obj, tsize=tsize, table=table, &
                               tomlName=tomlName, regions=regions, &
                               dom=dom)

CALL obj_ImportFromToml4_part2(obj=obj, tsize=tsize, table=table, &
                               tomlName=tomlName, regions=regions, &
                               dom=dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml4

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

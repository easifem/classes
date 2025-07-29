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

SUBMODULE(AbstractMaterial_Class) TomlMethods
USE GlobalData, ONLY: CHAR_LF, stdout
USE Display_Method, ONLY: Display, ToString
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_serialize, &
                 toml_array, &
                 toml_get => get_value, &
                 toml_len => len
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          ReadNameFromToml
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-29
! summary: This subroutine reads the name of AbstractMaterial_
!
! [material]
! --> name = "nameOfMaterial"
SUBROUTINE ReadNameFromToml(obj, table)
  CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadNameFromToml()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading name ...')
#endif

  CALL GetValue(table=table, key="name", VALUE=obj%name, &
                default_value=myprefix, origin=origin, &
                stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'Cannot find/read "name" in the config file.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadNameFromToml

!----------------------------------------------------------------------------
!                                                      ReadPropNamesFromToml
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-29
! summary: This subroutine reads the propNames of AbstractMaterial_
!
! [material]
! name = "nameOfMaterial"
! --> propNames = ["prop1", "prop2", "prop3"]

SUBROUTINE ReadPropNamesFromToml(obj, table, propNames)
  CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), ALLOCATABLE, INTENT(INOUT) :: propNames(:)

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadPropNamesFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, ii, tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading propNames ...')
#endif

  isok = ALLOCATED(propNames)
  IF (isok) THEN
    tsize = SIZE(propNames)
    DO ii = 1, tsize
      propNames(ii) = ""
    END DO
    DEALLOCATE (propNames)
  END IF

  CALL GetValue(table=table, key="propNames", VALUE=propNames, &
                origin=origin, stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'Cannot find/read "propNames" in the config file.')
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(propNames)
  CALL AssertError1(isok, myName, &
                    'Could not allocate "propNames" from the config file.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadPropNamesFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-29
! summary: This subroutine reads the userFucntion data from toml table
!
!# Introduction
!
! [material.prop1]
! name = "prop1"
! numReturns = 1 # number of returns, it is 1 for scalar
! argType = "Space"
! numArgs = 3 # number of arguments
! luaScript = "./hello.lua" # lua script
! luaFunctionName = "hello" #lua function
! value = 1.0 # constant scalar value

SUBROUTINE ReadDataFromTomlTable(obj, table, default_name)
  CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(IN) :: default_name

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadDataFromTomlTable()"
#endif

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  CLASS(UserFunction_), POINTER :: afunc
  TYPE(String) :: name

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

  CALL GetValue(table=table, key="name", VALUE=name, &
                default_value=default_name%chars(), origin=origin, &
                stat=stat, isFound=isok)

  CALL obj%AddMaterial(name=name%chars())
  afunc => NULL()
  afunc => obj%GetMaterialPointer(name=name%chars())

#ifdef DEBUG_VER
  isok = ASSOCIATED(afunc)
  CALL AssertError1(isok, myName, &
                    'Error while adding material name='//name//' to list.')
#endif

  CALL afunc%ImportFromToml(table=table)
  afunc => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadDataFromTomlTable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ReadMatPropsFromPropNames(obj, table, propNames)
  CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(IN) :: propNames(:)

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadMatPropsFromPropNames()"
#endif

  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat, tsize, ii
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

  tsize = SIZE(propNames)

  DO ii = 1, tsize

    node => NULL()
    CALL toml_get(table, propNames(ii)%chars(), node, origin=origin, &
                  requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
    isok = ASSOCIATED(node)
    CALL AssertError1(isok, myName, &
          'Cannot find/read propName='//propNames(ii)//' in the config file.')
#endif

    CALL ReadDataFromTomlTable(obj=obj, table=node, &
                               default_name=propNames(ii))
  END DO

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END]')
#endif

END SUBROUTINE ReadMatPropsFromPropNames

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ReadRegionFromToml(obj, table, region, dom)
  CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(MeshSelection_), OPTIONAL, INTENT(INOUT) :: region
  CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadRegionFromToml()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: origin, stat
  TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

  isok = PRESENT(region)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            'region is not present, nothing to do here.')
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  CALL toml_get(table, "region", node, &
                origin=origin, requested=.FALSE., stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
                    'region field is missing in toml table')
#endif

  CALL region%ImportFromToml(table=node, dom=dom)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadRegionFromToml

!----------------------------------------------------------------------------
!                                                           ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

TYPE(String), ALLOCATABLE :: propNames(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL ReadNameFromToml(obj=obj, table=table)
CALL ReadPropNamesFromToml(obj=obj, table=table, propNames=propNames)
CALL ReadMatPropsFromPropNames(obj=obj, table=table, propNames=propNames)
CALL ReadRegionFromToml(obj=obj, table=table, region=region, dom=dom)

IF (ALLOCATED(propNames)) DEALLOCATE (propNames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
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
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node, region=region, dom=dom)

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
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

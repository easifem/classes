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
!
!----------------------------------------------------------------------------

SUBROUTINE ReadDataFromTomlTable(obj, table)
  CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

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
                default_value=myprefix, origin=origin, &
                stat=stat, isFound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'Cannot find/read "name" in the config file.')
#endif

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

SUBROUTINE ImportFromTomlArray(obj, table)
  CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportFromTomlArray()"
#endif

  TYPE(toml_array), POINTER :: array
  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat, tsize, ii
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

  array => NULL()
  ! Try for an array of material tables
  CALL toml_get(table, toml_mat_prop_name, array, origin=origin, &
                requested=.FALSE., stat=stat)

  isok = ASSOCIATED(array)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    array => NULL()
    RETURN
  END IF

  tsize = toml_len(array)

  DO ii = 1, tsize
    node => NULL()
    CALL toml_get(array, ii, node)

#ifdef DEBUG_VER
    isok = ASSOCIATED(node)
    CALL AssertError1(isok, myName, &
         'Cannot find/read '//ToString(ii)//'th material in the config file.')
#endif

    CALL ReadDataFromTomlTable(obj=obj, table=node)
  END DO

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END]')
#endif

END SUBROUTINE ImportFromTomlArray

!----------------------------------------------------------------------------
!                                                         ImportFromTomlTable
!----------------------------------------------------------------------------

SUBROUTINE ImportFromTomlTable(obj, table)
  CLASS(AbstractMaterial_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportFromTomlTable()"
#endif

  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

  node => NULL()
  CALL toml_get(table, toml_mat_prop_name, node, origin=origin, &
                requested=.FALSE., stat=stat)
  isok = ASSOCIATED(node)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    node => NULL()
    RETURN
  END IF

  CALL ReadNameFromToml(obj=obj, table=node)
  CALL ReadDataFromTomlTable(obj=obj, table=node)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END]')
#endif

END SUBROUTINE ImportFromTomlTable

!----------------------------------------------------------------------------
!                                                           ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL ImportFromTomlTable(obj=obj, table=table)
CALL ImportFromTomlArray(obj=obj, table=table)

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

CALL obj%ImportFromToml(table=node)

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

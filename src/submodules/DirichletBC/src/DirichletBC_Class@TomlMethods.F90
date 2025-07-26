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

SUBMODULE(DirichletBC_Class) TomlMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_array, &
                 toml_stat
USE TomlUtility, ONLY: GetValue
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
TYPE(toml_array), POINTER :: array
LOGICAL(LGT) :: isok
INTEGER(I4B) :: origin, stat, tsize, ii, tsize1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading '//tomlName//' ...')
#endif

array => NULL()
CALL toml_get(table, tomlName, array, origin=origin, &
              requested=.FALSE., stat=stat)

isok = ASSOCIATED(array)

IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          tomlName//' not found, nothing to import.')
#endif
  RETURN
END IF

tsize = toml_len(array)

isok = ALLOCATED(obj)
IF (.NOT. isok) ALLOCATE (obj(tsize))

tsize1 = SIZE(obj)

#ifdef DEBUG_VER
isok = tsize .EQ. tsize1
CALL AssertError1(isok, myName, &
            '[CONFIG ERROR] :: The number of boundary condition '//CHAR_LF// &
                ' in the toml config ('//ToString(tsize)//') is not same '// &
                  ' as the size of obj ('//ToString(tsize1)//")")
#endif

DO ii = 1, tsize
  node => NULL()
  CALL toml_get(array, ii, node)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
                    '[CONFIG ERROR] :: DirichletBC '//ToString(ii)// &
                    ' cannot be read from the toml file.')
#endif

  isok = ASSOCIATED(obj(ii)%ptr)
  IF (.NOT. isok) ALLOCATE (obj(ii)%ptr)
  CALL obj(ii)%ptr%ImportFromToml(table=node, dom=dom)
END DO

node => NULL()
array => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
LOGICAL(LGT) :: isok
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

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
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")
#endif

CALL DirichletBCImportFromToml(obj=obj, table=table, dom=dom, &
                               tomlName=tomlName)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), myname//" Domain toml config: "// &
               CHAR_LF, unitno=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

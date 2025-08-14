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

SUBMODULE(AbstractDomain_Class) TomlMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE StringUtility, ONLY: GetExtension
USE tomlf, ONLY: toml_serialize, toml_get => get_value
USE TomlUtility, ONLY: GetValue

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                  ImportTotalMediumFromToml
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-12
! summary: Read total medium assigned to the domain

SUBROUTINE ImportTotalMediumFromToml(obj, table, totalMedium)
  CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: totalMedium

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportTotalMediumFromToml()"
#endif
  INTEGER(I4B), PARAMETER :: default_totalMedium = 1
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading totalMedium ...')
#endif

  CALL GetValue(table=table, key="totalMedium", VALUE=totalMedium, &
                origin=origin, stat=stat, isfound=isok, &
                default_value=default_totalMedium)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportTotalMediumFromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
LOGICAL(LGT) :: isok
CHARACTER(:), ALLOCATABLE :: ext
#endif

CHARACTER(*), PARAMETER :: default_meshfilename = "mesh.h5", &
                           default_group = ""

TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: origin, stat, totalMedium, ii, nsd
TYPE(String) :: meshfilename, group

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading filename ...')
#endif

CALL GetValue(table=table, key="filename", VALUE=meshfilename, &
              default_value=default_meshfilename, origin=origin, stat=stat)

#ifdef DEBUG_VER
ext = GetExtension(meshfilename%chars())
isok = ext .EQ. "h5"
CALL AssertError1(isok, myName, &
                  "The given filename is not a valid HDF5 file. "// &
                  "Extension should be 'h5'.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading group ...')
#endif

CALL GetValue(table=table, key="group", VALUE=group, &
              default_value=default_group, origin=origin, stat=stat)

CALL meshfile%Initiate(meshfilename%chars(), mode="READ")
CALL meshfile%OPEN()
CALL obj%IMPORT(hdf5=meshfile, group=group%chars())
CALL meshfile%DEALLOCATE()

CALL ImportTotalMediumFromToml(obj=obj, table=table, totalMedium=totalMedium)

nsd = obj%GetNSD()
DO ii = 1, nsd
  CALL obj%SetTotalMedium(dim=ii, n=totalMedium)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                '[CONFIG ERROR] :: following error occured while reading '// &
               'the toml file :: cannot find '//tomlName//" table in config.")
  RETURN
END IF

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
 CALL Display(toml_serialize(node), "AbstractDomain toml config: "//CHAR_LF, &
               unitno=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

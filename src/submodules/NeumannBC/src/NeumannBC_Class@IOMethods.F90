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

SUBMODULE(NeumannBC_Class) IOMethods
USE BaseMethod
USE TomlUtility
USE tomlf, ONLY:  &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_array,  &
  & toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "bc_ImportFromToml1()"
TYPE(toml_table), POINTER :: node
TYPE(toml_array), POINTER :: array
INTEGER(I4B) :: origin, stat, tsize, ii, tsize1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportFromToml()')
#endif

tsize1 = SIZE(obj)

array => NULL()
CALL toml_get(table, tomlName, array, origin=origin, &
  & requested=.FALSE., stat=stat)

IF (.NOT. ASSOCIATED(array)) THEN
  IF (tsize1 .GT. 0_I4B) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & 'In toml file :: cannot find ['//tomlName//"] table.")
  ELSE
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
      & 'In toml file :: cannot find ['//tomlName//"] table.")
  END IF
  RETURN
END IF

tsize = toml_len(array)
IF (tsize .NE. tsize1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: The number of boundary condition '//  &
    & ' in the toml config is not same as the size of obj')
  RETURN
END IF

DO ii = 1, tsize
  node => NULL()
  CALL toml_get(array, ii, node)
  IF (.NOT. ASSOCIATED(node)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: NeumannBC '//tostring(ii)//  &
      & ' cannot be read from the toml file.')
  END IF
  IF (.NOT. ASSOCIATED(obj(ii)%ptr)) ALLOCATE (obj(ii)%ptr)
  CALL obj(ii)%ptr%ImportFromToml(table=node, dom=dom)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportParamFromToml()')
#endif
END PROCEDURE bc_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "bc_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportFromToml()')
#endif

IF (PRESENT(afile)) THEN
  CALL GetValue(table=table, afile=afile)
ELSEIF (PRESENT(filename)) THEN
  CALL GetValue(table=table, filename=filename)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: either filename or afile should be present!')
  RETURN
END IF

CALL NeumannBCImportFromToml(obj=obj, table=table, dom=dom,  &
  & tomlName=tomlName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportParamFromToml()')
#endif
END PROCEDURE bc_ImportFromToml2
END SUBMODULE IOMethods

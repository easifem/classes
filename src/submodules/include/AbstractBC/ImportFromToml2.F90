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
!

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: This macro is used in the AbstractBC child classes's tomlMethods
!
! Interface is given below
!
! MODULE SUBROUTINE obj_ImportFromToml2( &
!   obj, dom, tomlName, afile, filename, printToml)
!   TYPE(DirichletBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
!   CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
!   CHARACTER(*), INTENT(IN) :: tomlName
!   TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
!   CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
!   LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
! END SUBROUTINE obj_ImportFromToml2
!
! In the submodule you should define myName in dver
!
! Submodule should contain the following use statements
!
! USE BaseType, ONLY: math => TypeMathOpt
! USE tomlf, ONLY: toml_get => get_value
! USE tomlf, ONLY: toml_len => len
! USE tomlf, ONLY: toml_array
! USE TomlUtility, ONLY: GetValue
!
! Make sure to define _IMPORT_FROM_TOML_ in the calling program
! see line 79 of this file
! For example DirichletBCImportFromToml, NeumannBCImportFromToml etc

#ifdef DEBUG_VER
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

CALL _IMPORT_FROM_TOML_( &
  obj=obj, table=table, dom=dom, tomlName=tomlName)

node => NULL()
DEALLOCATE (table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

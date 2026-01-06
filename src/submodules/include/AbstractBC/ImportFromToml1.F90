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

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: This macro is used in the AbstractBC child classes's tomlMethods

! interface is given below
! INTERFACE
!   MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom, tomlName)
!     TYPE(DirichletBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
!     !! Should be allocated outside
!     TYPE(toml_table), INTENT(INOUT) :: table
!     !! Toml table to returned
!     CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
!     !! domain
!     CHARACTER(*), INTENT(IN) :: tomlName
!   END SUBROUTINE obj_ImportFromToml1
! END INTERFACE

! Add following use statement in the submodule
! USE Display_Method, ONLY: ToString
! USE tomlf, ONLY: toml_get => get_value
! USE tomlf, ONLY: toml_len => len
! USE tomlf, ONLY: toml_array
! USE BaseType, ONLY: math => TypeMathOpt

! In case of NeumannBC we often call SetElemToLocalBoundary() method.
! To add that line define the following macro in the calling file.
! _POST_IMPORT_FROM_TOML_CALL_STMT_
! For example
! #define _POST_IMPORT_FROM_TOML_CALL_STMT_
! CALL obj(ii)%ptr%SetElemToLocalBoundary()

! MODULE PROCEDURE obj_ImportFromToml1
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
! #endif

TYPE(toml_table), POINTER :: node
TYPE(toml_array), POINTER :: array
LOGICAL(LGT) :: isok
INTEGER(I4B) :: origin, stat, tsize, ii, tsize1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading '//tomlName//' ...')
#endif

array => NULL()
CALL toml_get(table, tomlName, array, origin=origin, &
              requested=math%no, stat=stat)

isok = ASSOCIATED(array)

IF (.NOT. isok) THEN
  ALLOCATE (obj(0))

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    tomlName//' not found, nothing to import.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

tsize = toml_len(array)

isok = ALLOCATED(obj)
IF (.NOT. isok) ALLOCATE (obj(tsize))

tsize1 = SIZE(obj)

#ifdef DEBUG_VER
isok = tsize .EQ. tsize1
CALL AssertError1( &
  isok, myName, 'The number of boundary condition in the toml config ('// &
 ToString(tsize)//') is not same as the size of obj ('//ToString(tsize1)//")")
#endif

DO ii = 1, tsize
  node => NULL()
  CALL toml_get(array, ii, node)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
    'boundary condition no. '//ToString(ii)//' cannot be read from the &
    &toml file.')
#endif

  isok = ASSOCIATED(obj(ii)%ptr)
  IF (.NOT. isok) ALLOCATE (obj(ii)%ptr)
  CALL obj(ii)%ptr%ImportFromToml(table=node, dom=dom)

#ifdef _POST_IMPORT_FROM_TOML_CALL_STMT_
  CALL obj(ii)%ptr%_POST_IMPORT_FROM_TOML_CALL_STMT_
#endif

END DO

node => NULL()
array => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

! END PROCEDURE obj_ImportFromToml1

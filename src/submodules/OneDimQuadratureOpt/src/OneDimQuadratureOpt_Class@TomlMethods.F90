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

SUBMODULE(OneDimQuadratureOpt_Class) TomlMethods
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToInteger
USE tomlf, ONLY: toml_get => get_value
USE TomlUtility, ONLY: GetValue

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: origin, stat, order, nips, quadratureType
LOGICAL(LGT) :: isFound, isOrder, isNips
TYPE(String) :: quadratureType_char
REAL(DFP) :: alpha, beta, lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading quadratureType...')
#endif

CALL GetValue( &
  table=table, key="quadratureType", VALUE=quadratureType_char, &
  default_value=TypeOneDimQuadratureOpt%quadratureType_char, &
  origin=origin, stat=stat, isFound=isFound)

quadratureType = QuadraturePoint_ToInteger(quadratureType_char%chars())

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading order...')
#endif

CALL GetValue( &
  table=table, key="order", VALUE=order, &
  default_value=TypeOneDimQuadratureOpt%order, origin=origin, stat=stat, &
  isFound=isOrder)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading nips...')
#endif

CALL GetValue( &
  table=table, key="nips", VALUE=nips, &
  default_value=TypeOneDimQuadratureOpt%nips(1), origin=origin, stat=stat, &
  isFound=isNips)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading alpha...')
#endif

CALL GetValue( &
  table=table, key="alpha", VALUE=alpha, &
  default_value=TypeOneDimQuadratureOpt%alpha, origin=origin, stat=stat, &
  isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading beta...')
#endif

CALL GetValue( &
  table=table, key="beta", VALUE=beta, &
  default_value=TypeOneDimQuadratureOpt%beta, origin=origin, stat=stat, &
  isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Reading lambda...')
#endif

CALL GetValue( &
  table=table, key="lambda", VALUE=lambda, &
  default_value=TypeOneDimQuadratureOpt%lambda, origin=origin, stat=stat, &
  isFound=isFound)

! Here we call initiate methods with above parameters
CALL obj%Initiate( &
  quadratureType=quadratureType, order=order, nips=nips, alpha=alpha, &
  beta=beta, lambda=lambda, isOrder=isOrder, isNips=isNips)

! clean up
quadratureType_char = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
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
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods


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
USE TomlUtility, ONLY: GetValue, GetValue_
USE ReferenceLine_Method, ONLY: RefCoord_Line

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                ImportQuadratureTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportQuadratureTypeFromToml(obj, table, origin, stat)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportQuadratureTypeFromToml()"
#endif

  TYPE(String) :: astr
  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="quadratureType", VALUE=astr, &
    default_value=TypeOneDimQuadratureOpt%quadratureType_char, &
    origin=origin, stat=stat, isFound=isFound)

  obj%quadratureType = QuadraturePoint_ToInteger(astr%chars())
  obj%quadratureType_char = astr%chars()

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportQuadratureTypeFromToml

!----------------------------------------------------------------------------
!                                                        ImportOrderFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportOrderFromToml(obj, table, origin, stat)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportOrderFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="order", VALUE=obj%order, origin=origin, stat=stat, &
    default_value=TypeOneDimQuadratureOpt%order, isFound=obj%isOrder)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportOrderFromToml

!----------------------------------------------------------------------------
!                                                         ImportNipsFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportNipsFromToml(obj, table, origin, stat)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportNipsFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="nips", VALUE=obj%nips(1), origin=origin, stat=stat, &
    default_value=TypeOneDimQuadratureOpt%nips(1), isFound=obj%isNips)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportNipsFromToml

!----------------------------------------------------------------------------
!                                                        ImportAlphaFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportAlphaFromToml(obj, table, origin, stat)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportAlphaFromToml()"
#endif

  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="alpha", VALUE=obj%alpha, origin=origin, stat=stat, &
    default_value=TypeOneDimQuadratureOpt%alpha, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportAlphaFromToml

!----------------------------------------------------------------------------
!                                                         ImportBetaFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBetaFromToml(obj, table, origin, stat)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBetaFromToml()"
#endif

  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="beta", VALUE=obj%beta, origin=origin, stat=stat, &
    default_value=TypeOneDimQuadratureOpt%beta, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportBetaFromToml

!----------------------------------------------------------------------------
!                                                       ImportlambdaFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportLambdaFromToml(obj, table, origin, stat)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportLambdaFromToml()"
#endif

  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="lambda", VALUE=obj%lambda, origin=origin, stat=stat, &
    default_value=TypeOneDimQuadratureOpt%lambda, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportLambdaFromToml

!----------------------------------------------------------------------------
!                                                 ImportRefelemDomainFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportRefelemDomainFromToml(obj, table, origin, stat)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportRefelemDomainFromToml()"
#endif

  TYPE(String) :: astr
  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="refelemDomain", VALUE=astr, origin=origin, stat=stat, &
    default_value=TypeOneDimQuadratureOpt%refelemDomain, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportRefelemDomainFromToml

!----------------------------------------------------------------------------
!                                                 ImportRefelemCoordFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportRefelemCoordFromToml(obj, table, origin, stat)
  CLASS(OneDimQuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportRefelemCoordFromToml()"
#endif

  LOGICAL(LGT) :: isFound
  REAL(DFP) :: refelemCoord(3, 8)
  INTEGER(I4B) :: nrow, ncol

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_( &
    table=table, key="refelemCoord", VALUE=refelemCoord, &
    origin=origin, stat=stat, nrow=nrow, ncol=ncol, &
    isFound=isFound)

  IF (.NOT. isFound) THEN
    refelemCoord(1:1, 1:2) = RefCoord_Line(obj%refelemDomain)
  END IF

  obj%refelemCoord(1:1, 1:2) = refelemCoord(1:1, 1:2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE ImportRefelemCoordFromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL ImportQuadratureTypeFromToml(obj, table, origin, stat)
CALL ImportOrderFromToml(obj, table, origin, stat)
CALL ImportNipsFromToml(obj, table, origin, stat)
CALL ImportAlphaFromToml(obj, table, origin, stat)
CALL ImportBetaFromToml(obj, table, origin, stat)
CALL ImportLambdaFromToml(obj, table, origin, stat)
CALL ImportRefelemDomainFromToml(obj, table, origin, stat)
CALL ImportRefelemCoordFromToml(obj, table, origin, stat)

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

NULLIFY (node)
DEALLOCATE (table)

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


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

SUBMODULE(OneDimBasisOpt_Class) TomlMethods
USE String_Class, ONLY: String
USE tomlf, ONLY: toml_get => get_value
USE TomlUtility, ONLY: GetValue
USE FEVariable_Method, ONLY: FEVariable_ToInteger
USE BaseInterpolation_Method, ONLY: BaseType_ToInteger
USE BaseInterpolation_Method, ONLY: InterpolationPoint_ToInteger

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                             ImportBaseInterpolationFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBaseInterpolationFromToml(obj, table, origin, stat, &
                                           baseInterpolation, isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  TYPE(String), INTENT(INOUT) :: baseInterpolation
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBaseInterpolationFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="baseInterpolation", VALUE=baseInterpolation, &
    default_value=TypeOneDimBasisOpt%baseInterpolation, origin=origin, &
    stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBaseInterpolationFromToml

!----------------------------------------------------------------------------
!                                                ImportBaseContinuityFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBaseContinuityFromToml(obj, table, origin, stat, &
                                        baseContinuity, isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  TYPE(String), INTENT(INOUT) :: baseContinuity
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBaseContinuityFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="baseContinuity", VALUE=baseContinuity, &
    default_value=TypeOneDimBasisOpt%baseContinuity, origin=origin, &
    stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBaseContinuityFromToml

!----------------------------------------------------------------------------
!                                                        ImportIpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportIpTypeFromToml(obj, table, origin, stat, ipType, &
                                isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  INTEGER(I4B), INTENT(INOUT) :: ipType
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportIpTypeFromToml()"
#endif

  TYPE(String) :: ipType_char

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="ipType", VALUE=ipType_char, &
    default_value=TypeOneDimBasisOpt%ipType_char, origin=origin, stat=stat, &
    isFound=isFound)

  ipType = InterpolationPoint_ToInteger(ipType_char%chars())

  ipType_char = ''

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportIpTypeFromToml

!----------------------------------------------------------------------------
!                                                     ImportBasisTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBasisTypeFromToml(obj, table, origin, stat, basisType, &
                                   isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  INTEGER(I4B), INTENT(INOUT) :: basisType
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBasisTypeFromToml()"
#endif

  TYPE(String) :: basisType_char

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="basisType", VALUE=basisType_char, &
    default_value=TypeOneDimBasisOpt%basisType_char, origin=origin, &
    stat=stat, isFound=isFound)
  basisType = BaseType_ToInteger(basisType_char%chars())

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBasisTypeFromToml

!----------------------------------------------------------------------------
!                                                         ImportAlphaFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportAlphaFromToml(obj, table, origin, stat, alpha, isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  REAL(DFP), INTENT(INOUT) :: alpha
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportAlphaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="alpha", VALUE=alpha, origin=origin, stat=stat, &
    default_value=TypeOneDimBasisOpt%alpha, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportAlphaFromToml

!----------------------------------------------------------------------------
!                                                          ImportBetaFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBetaFromToml(obj, table, origin, stat, beta, isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  REAL(DFP), INTENT(INOUT) :: beta
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBetaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="beta", VALUE=beta, origin=origin, stat=stat, &
    default_value=TypeOneDimBasisOpt%beta, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBetaFromToml

!----------------------------------------------------------------------------
!                                                        ImportLambdaFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportLambdaFromToml(obj, table, origin, stat, lambda, isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  REAL(DFP), INTENT(INOUT) :: lambda
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportLambdaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="lambda", VALUE=lambda, origin=origin, stat=stat, &
    default_value=TypeOneDimBasisOpt%lambda, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportLambdaFromToml

!----------------------------------------------------------------------------
!                                                         ImportOrderFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportOrderFromToml(obj, table, origin, stat, order, isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  INTEGER(I4B), INTENT(INOUT) :: order
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportOrderFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="order", VALUE=order, origin=origin, stat=stat, &
    default_value=TypeOneDimBasisOpt%order, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportOrderFromToml

!----------------------------------------------------------------------------
!                                                        ImportFeTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportFeTypeFromToml(obj, table, origin, stat, feType, isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  INTEGER(I4B), INTENT(INOUT) :: feType
  LOGICAL(LGT), INTENT(INOUT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportFeTypeFromToml()"
#endif

  TYPE(String) :: feType_char

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="feType", VALUE=feType_char, origin=origin, stat=stat, &
    default_value=TypeOneDimBasisOpt%feType_char, isFound=isFound)

  feType = FEVariable_ToInteger(feType_char%chars())

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportFeTypeFromToml

!----------------------------------------------------------------------------
!                                                       ImportQuadOptFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportQuadOptFromToml(obj, table, origin, stat, isFound)
  CLASS(OneDimBasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  LOGICAL(LGT), INTENT(INOUT) :: isFound

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportQuadOptFromToml()"
#endif

  CHARACTER(*), PARAMETER :: default_quadOptName = "quadOpt"
  LOGICAL(LGT) :: isok
  TYPE(String) :: astr
  TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="quadOptName", VALUE=astr, origin=origin, stat=stat, &
    default_value=default_quadOptName, isFound=isFound)

#ifdef DEBUG_VER
  IF (.NOT. isFound) THEN
    CALL e%raiseDebug(modName//'::'//myName//' - '// &
       'quadOptName not found in toml, proceeding with default value: '//astr)
  END IF
#endif

  !! Get the node from toml table with name quadOptName
  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
                stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  IF (.NOT. isok) THEN
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'Node with name '//astr//' is not associated'// &
                      ' quadOpt will not be imported.')

    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')

    RETURN
  END IF
#endif

  CALL obj%quadOpt%ImportFromToml(table=node)

  node => NULL()
  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportQuadOptFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: origin, stat, order, ipType, basisType, feType
LOGICAL(LGT) :: isFound
TYPE(String) :: baseContinuity, baseInterpolation
REAL(DFP) :: alpha, beta, lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ImportBaseInterpolationFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, &
  baseInterpolation=baseInterpolation, isFound=isFound)

CALL ImportBaseContinuityFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, &
  baseContinuity=baseContinuity, isFound=isFound)

CALL ImportIpTypeFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, ipType=ipType, &
  isFound=isFound)

CALL ImportBasisTypeFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, basisType=basisType, &
  isFound=isFound)

CALL ImportAlphaFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, alpha=alpha, &
  isFound=isFound)

CALL ImportBetaFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, beta=beta, &
  isFound=isFound)

CALL ImportLambdaFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, lambda=lambda, &
  isFound=isFound)

CALL ImportOrderFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, order=order, &
  isFound=isFound)

CALL ImportFeTypeFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, feType=feType, &
  isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Initiating obj without quadOpt...')
#endif

CALL obj%Initiate( &
  baseContinuity=baseContinuity%chars(), &
  baseInterpolation=baseInterpolation%chars(), &
  feType=feType, &
  ipType=ipType, &
  basisType=basisType, &
  alpha=alpha, beta=beta, lambda=lambda, &
  order=order)

CALL ImportQuadOptFromToml( &
  obj=obj, table=table, origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
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

node => NULL()
DEALLOCATE (table)

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

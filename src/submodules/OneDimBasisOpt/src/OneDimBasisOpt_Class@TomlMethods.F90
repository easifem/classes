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
USE GlobalData, ONLY: stdout, CHAR_LF
USE String_Class, ONLY: String
USE Display_Method, ONLY: ToString, Display
USE BaseType, ONLY: TypeFEVariableOpt, &
                    elemNameOpt => TypeElemNameOpt
USE FPL_Method, ONLY: GetValue, CheckEssentialParam, Set
USE StringUtility, ONLY: UpperCase
USE InputUtility, ONLY: Input
USE LineInterpolationUtility, ONLY: RefElemDomain_Line
USE ReferenceLine_Method, ONLY: RefCoord_Line
USE BaseInterpolation_Method, ONLY: BaseType_ToChar, &
                                    BaseType_ToInteger, &
                                    InterpolationPoint_ToChar, &
                                    InterpolationPoint_ToInteger
USE QuadraturePoint_Method, ONLY: QuadratureCopy => Copy, &
                                  QuadraturePointDisplay => Display, &
                                  QuadraturePointInitiate => Initiate
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE TomlUtility, ONLY: GetValue
USE FEVariable_Method, ONLY: FEVariable_ToInteger

IMPLICIT NONE

#define quadOptPrefix "quadratureOpt"

CONTAINS

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
! Internal variables
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
INTEGER(I4B) :: origin, stat, order, ipType, basisType, feType
LOGICAL(LGT) :: isFound
TYPE(String) :: baseContinuity, baseInterpolation, &
                ipType_char, basisType_char, feType_char
REAL(DFP) :: alpha, beta, lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseInterpolation...')
#endif

CALL GetValue(table=table, key="baseInterpolation", &
              VALUE=baseInterpolation, &
              default_value=TypeOneDimBasisOpt%baseInterpolation, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading baseContinuity...')
#endif

CALL GetValue(table=table, key="baseContinuity", &
              VALUE=baseContinuity, &
              default_value=TypeOneDimBasisOpt%baseContinuity, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading ipType...')
#endif

CALL GetValue(table=table, key="ipType", &
              VALUE=ipType_char, &
              default_value=TypeOneDimBasisOpt%ipType_char, &
              origin=origin, stat=stat, isFound=isFound)
ipType = InterpolationPoint_ToInteger(ipType_char%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading basisType...')
#endif

CALL GetValue(table=table, key="basisType", &
              VALUE=basisType_char, &
              default_value=TypeOneDimBasisOpt%basisType_char, &
              origin=origin, stat=stat, isFound=isFound)
basisType = BaseType_ToInteger(basisType_char%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading alpha...')
#endif

CALL GetValue(table=table, key="alpha", &
              VALUE=alpha, &
              default_value=TypeOneDimBasisOpt%alpha, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading beta...')
#endif

CALL GetValue(table=table, key="beta", &
              VALUE=beta, &
              default_value=TypeOneDimBasisOpt%beta, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading lambda...')
#endif

CALL GetValue(table=table, key="lambda", &
              VALUE=lambda, &
              default_value=TypeOneDimBasisOpt%lambda, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading order...')
#endif

CALL GetValue(table=table, key="order", &
              VALUE=order, &
              default_value=TypeOneDimBasisOpt%order, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading feType...')
#endif

CALL GetValue(table=table, key="feType", &
              VALUE=feType_char, &
              default_value=TypeOneDimBasisOpt%feType_char, &
              origin=origin, stat=stat, isFound=isFound)
feType = FEVariable_ToInteger(feType_char%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Initiating obj without quadOpt...')
#endif

CALL obj%Initiate(baseContinuity=baseContinuity%chars(), &
                  baseInterpolation=baseInterpolation%chars(), &
                  ipType=ipType, &
                  basisType=basisType, &
                  alpha=alpha, &
                  beta=beta, &
                  lambda=lambda, &
                  order=order, &
                  fetype=fetype)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading quadOpt...')
#endif

CALL obj%quadOpt%ImportFromToml(table=table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
! internal variables
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
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

isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
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

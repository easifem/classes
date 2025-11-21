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

SUBMODULE(OneDimBasisOpt_Class) Methods
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
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
! Internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = "obj_Initiate2()"
#endif

TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL obj%SetParam( &
  baseContinuity=baseContinuity, &
  baseInterpolation=baseInterpolation, &
  ipType=ipType, &
  basisType=basisType, &
  alpha=alpha, &
  beta=beta, &
  lambda=lambda, &
  order=order, &
  fetype=fetype, &
  quadratureType=quadratureType, &
  quadratureOrder=quadratureOrder, &
  quadratureNips=quadratureNips, &
  quadratureAlpha=quadratureAlpha, &
  quadratureBeta=quadratureBeta, &
  quadratureLambda=quadratureLambda)

obj%tdof = obj%order + 1

astr = RefElemDomain_Line(baseContinuity=baseContinuity, &
                          baseInterpol=baseInterpolation)
obj%refelemDomain = astr%slice(1, 1)
astr = ""
obj%refelemCoord(1:1, 1:2) = RefCoord_Line(obj%refelemDomain)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%Copy(TypeOneDimBasisOpt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Do not call obj%Deallocate(), here,
! because in obj%Deallocate() we are calling obj%Copy for
! resetting the object

! Copy the values from obj2 to obj
obj%firstCall = obj2%firstCall
obj%tdof = obj2%tdof
obj%order = obj2%order
obj%fetype = obj2%fetype
obj%ipType = obj2%ipType
obj%basisType = obj2%basisType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%refelemDomain = obj2%refelemDomain
obj%baseContinuity = obj2%baseContinuity
obj%baseInterpolation = obj2%baseInterpolation
obj%refelemCoord = obj2%refelemCoord
obj%basisType_char = obj2%basisType_char
obj%ipType_char = obj2%ipType_char

CALL obj%quadOpt%Copy(obj2%quadOpt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg=msg, unitno=unitno)
CALL Display(obj%firstCall, msg="firstCall: ", unitno=unitno)
CALL Display(obj%tdof, msg="tdof: ", unitno=unitno)
CALL Display(obj%order, msg="order: ", unitno=unitno)
CALL Display(obj%feType, msg="feType: ", unitno=unitno)
CALL Display(obj%ipType, msg="ipType: ", unitno=unitno)
CALL Display(obj%basisType, msg="basisType: ", unitno=unitno)
CALL Display(obj%alpha, msg="alpha: ", unitno=unitno)
CALL Display(obj%beta, msg="beta: ", unitno=unitno)
CALL Display(obj%lambda, msg="lambda: ", unitno=unitno)
CALL Display(obj%refElemDomain, msg="refElemDomain: ", unitno=unitno)
CALL Display(obj%baseContinuity, msg="baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation, msg="baseInterpolation: ", &
             unitno=unitno)
CALL Display(obj%refelemCoord, msg="refelemCoord: ", unitno=unitno)
CALL Display(TRIM(obj%basisType_char), msg="basisType_char: ", &
             unitno=unitno)
CALL Display(TRIM(obj%ipType_char), msg="ipType_char: ", &
             unitno=unitno)

CALL obj%quadOpt%Display(msg="QuadratureOpt: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                   SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
! Internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(order)) obj%order = order
IF (PRESENT(feType)) obj%feType = feType
IF (PRESENT(ipType)) obj%ipType = ipType
  IF (PRESENT(baseContinuity)) obj%baseContinuity = UpperCase(baseContinuity(1:2))
IF (PRESENT(baseInterpolation)) obj%baseInterpolation = &
  UpperCase(baseInterpolation(1:4))
IF (PRESENT(refElemDomain)) obj%refElemDomain = &
  UpperCase(refElemDomain(1:1))
IF (PRESENT(basisType)) obj%basisType = basisType
IF (PRESENT(alpha)) obj%alpha = alpha
IF (PRESENT(beta)) obj%beta = beta
IF (PRESENT(lambda)) obj%lambda = lambda
IF (PRESENT(firstCall)) obj%firstCall = firstCall

IF (PRESENT(dofType)) obj%dofType = dofType
IF (PRESENT(transformType)) obj%transformType = transformType

CALL obj%quadOpt%SetParam( &
  quadratureType=quadratureType, order=quadratureOrder, &
  nips=quadratureNips, alpha=quadratureAlpha, beta=quadratureBeta, &
  lambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                              SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%order = order
obj%tdof = order + 1
obj%firstCall = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                                  GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(order)) order = obj%order
IF (PRESENT(tdof)) tdof = obj%tdof
IF (PRESENT(feType)) feType = obj%feType
IF (PRESENT(ipType)) ipType = obj%ipType
IF (PRESENT(baseContinuity)) baseContinuity = obj%baseContinuity
IF (PRESENT(baseInterpolation)) baseInterpolation = obj%baseInterpolation
IF (PRESENT(refElemDomain)) refElemDomain = obj%refElemDomain
IF (PRESENT(basisType)) basisType = obj%basisType
IF (PRESENT(alpha)) alpha = obj%alpha
IF (PRESENT(beta)) beta = obj%beta
IF (PRESENT(lambda)) lambda = obj%lambda
IF (PRESENT(firstCall)) firstCall = obj%firstCall
IF (PRESENT(dofType)) dofType = obj%dofType
IF (PRESENT(transformType)) transformType = obj%transformType

CALL obj%quadOpt%GetParam( &
  quadratureType=quadratureType, order=quadratureOrder, &
  nips=quadratureNips, alpha=quadratureAlpha, &
  beta=quadratureBeta, lambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                         GetBaseContinuity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseContinuity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseContinuity()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%baseContinuity

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBaseContinuity

!----------------------------------------------------------------------------
!                                                      GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseInterpolation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseInterpolation()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%baseInterpolation

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                       GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
INTEGER(I4B) :: order0, quadratureType0
REAL(DFP) :: alpha0, beta0, lambda0

! Let us update the quadrature points quadOpt
CALL obj%quadOpt%SetParam(quadratureType=quadratureType, &
                          order=order, alpha=alpha, beta=beta, lambda=lambda)

! Let us get the quadrature points
CALL obj%quadOpt%GetParam(quadratureType=quadratureType0, &
                       order=order0, alpha=alpha0, beta=beta0, lambda=lambda0)

! Internal variables
CALL QuadraturePointInitiate(obj=quad, &
                             elemType=elemNameOpt%line, &
                             domainName=obj%refelemDomain, &
                             order=order0, &
                             quadratureType=quadratureType0, &
                             alpha=alpha0, &
                             beta=beta0, &
                             lambda=lambda0)

END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                   GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%quadOpt%GetTotalQuadraturePoints()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                                             GetCaseName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCaseName
ans = obj%baseContinuity//obj%baseInterpolation
END PROCEDURE obj_GetCaseName

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
ans = obj%order
END PROCEDURE obj_GetOrder

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary: Import OneDimBasisOpt_ from toml table
!
!# Introduction
! The toml table should have following contents:
!
!```toml
! [BasisOpt]
!
!```

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

END SUBMODULE Methods

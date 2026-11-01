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

SUBMODULE(BasisOpt_Class) TomlMethods
USE BaseInterpolation_Method, ONLY: BaseType_ToInteger
USE BaseInterpolation_Method, ONLY: InterpolationPoint_ToInteger
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE FEVariable_Method, ONLY: FEVariable_ToInteger
USE InterpolationUtility, ONLY: RefElemDomain
USE ReferenceElement_Method, ONLY: ElementTopology
USE ReferenceElement_Method, ONLY: ElementType
USE ReferenceElement_Method, ONLY: GetElementIndex
USE ReferenceElement_Method, ONLY: RefCoord_
USE ReferenceElement_Method, ONLY: XiDimension
USE StringUtility, ONLY: UpperCase
USE tomlf, ONLY: toml_get => get_value
USE TomlUtility, ONLY: GetValue
USE TomlUtility, ONLY: GetValue_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "BasisOpt_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif
INTEGER(I4B) :: ii, jj, STAT, origin
LOGICAL(LGT) :: isok
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.

CALL ImportBaseInterpolationFromToml(obj=obj, table=table, &
                                     origin=origin, STAT=STAT)
CALL ImportBaseContinuityFromToml(obj=obj, table=table, &
                                  origin=origin, STAT=STAT)
CALL ImportIpTypeFromToml(obj=obj, table=table, origin=origin, STAT=STAT)
CALL ImportBasisTypeFromToml(obj=obj, table=table, origin=origin, STAT=STAT)
CALL ImportAlphaFromToml(obj=obj, table=table, origin=origin, STAT=STAT)
CALL ImportBetaFromToml(obj=obj, table=table, origin=origin, STAT=STAT)
CALL ImportLambdaFromToml(obj=obj, table=table, origin=origin, STAT=STAT)
CALL ImportBrokenSpaceFromToml(obj=obj, table=table, &
                               origin=origin, STAT=STAT)
CALL ImportFeTypeFromToml(obj=obj, table=table, origin=origin, STAT=STAT)
CALL ImportElemTypeFromToml(obj=obj, table=table, origin=origin, STAT=STAT)

isok = PRESENT(nsd)
IF (isok) obj%nsd = nsd

isok = PRESENT(elemType)
IF (isok) THEN
  obj%elemType = elemType
  obj%topoType = ElementTopology(elemType)
  obj%xidim = XiDimension(obj%topoType)
  astr = RefElemDomain(elemType=obj%topoType, &
                       baseContinuity=obj%baseContinuity, &
                       baseInterpol=obj%baseInterpolation)
  obj%refelemDomain = astr%Slice(1, 1)
  astr = ""
  CALL RefCoord_(elemType=obj%topoType, ans=obj%refelemCoord, &
                 nrow=ii, ncol=jj, refelem=obj%refelemDomain)
  obj%elemIndx = GetElementIndex(obj%topoType)
END IF

CALL ImportQuadOptFromToml(obj=obj, table=table, origin=origin, STAT=STAT)

astr = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                           ElemTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportElemTypeFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  INTEGER(I4B) :: ii, jj
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportElemTypeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="elemType", VALUE=astr, &
                default_value="NA", &
                origin=origin, STAT=STAT, isFound=isFound)

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    astr = ""
    RETURN
  END IF

  ! Convert the string to integer
  obj%elemType = ElementType(astr%chars())
  obj%topoType = ElementTopology(obj%elemType)
  obj%xidim = XiDimension(obj%topoType)
  astr = RefElemDomain(elemType=obj%topoType, &
                       baseContinuity=obj%baseContinuity, &
                       baseInterpol=obj%baseInterpolation)
  obj%refelemDomain = astr%Slice(1, 1)
  astr = ""
  CALL RefCoord_(elemType=obj%topoType, ans=obj%refelemCoord, &
                 nrow=ii, ncol=jj, refelem=obj%refelemDomain)
  obj%elemIndx = GetElementIndex(obj%topoType)

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportElemTypeFromToml

!----------------------------------------------------------------------------
!                                                        QuadOptNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportQuadOptFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound, isok
  CHARACTER(*), PARAMETER :: default_quadOptName = "quadOpt"
  TYPE(String) :: astr
  TYPE(toml_table), POINTER :: node

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "QuadOptNameFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="quadOptName", VALUE=astr, &
                default_value=default_quadOptName, &
                origin=origin, STAT=STAT, isFound=isFound)

#ifdef DEBUG_VER
  IF (.NOT. isFound) THEN
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'quadOptName not found in toml, using default: '//astr)
  END IF
#endif

  !! Get the node from toml table with name quadOptName
  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=.FALSE., STAT=STAT)

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

  CALL obj%quadOpt%ImportFromToml(table=node, topoType=obj%topoType, &
                                  nsd=obj%nsd, xidim=obj%xidim, &
                                  refelemDomain=obj%refelemDomain, &
                                  refelemCoord=obj%refelemCoord)

  node => NULL()
  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportQuadOptFromToml

!----------------------------------------------------------------------------
!                                                         HelpImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBaseInterpolationFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBaseInterpolationFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="baseInterpolation", VALUE=astr, &
                default_value=TypeBasisOpt%baseInterpolation, &
                origin=origin, STAT=STAT, isFound=isFound)

  obj%baseInterpolation = UpperCase(astr%slice(1, 4))

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBaseInterpolationFromToml

!----------------------------------------------------------------------------
!                                                         HelpImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBaseContinuityFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBaseContinuityFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="baseContinuity", &
                VALUE=astr, &
                default_value=TypeBasisOpt%baseContinuity, &
                origin=origin, STAT=STAT, isFound=isFound)

  obj%baseContinuity = UpperCase(astr%slice(1, 2))

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBaseContinuityFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportIpTypeFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportIpTypeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="ipType", VALUE=astr, &
                default_value=TypeBasisOpt%ipType_char, &
                origin=origin, STAT=STAT, isFound=isFound)
  obj%ipType_char = astr%chars()
  obj%ipType = InterpolationPoint_ToInteger(astr%chars())

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportIpTypeFromToml

!----------------------------------------------------------------------------
!                                                              FeTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportFeTypeFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportFeTypeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="feType", VALUE=astr, &
                default_value=TypeBasisOpt%feType_char, &
                origin=origin, STAT=STAT, isFound=isFound)
  obj%feType_char = astr%chars()
  obj%feType = FEVariable_ToInteger(astr%chars())

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportFeTypeFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBasisTypeFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound, isBasisTypeScalar
  TYPE(String) :: astr(3)
  INTEGER(I4B) :: tBasisType, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBasisTypeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="basisType", &
                 VALUE=astr, tsize=tBasisType, &
                 isScalar=isBasisTypeScalar, &
                 origin=origin, STAT=STAT, isFound=isFound)

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif

    RETURN
  END IF

  IF (isBasisTypeScalar) THEN
    obj%basisType = BaseType_ToInteger(astr(1)%chars())
  ELSE
    tsize = MIN(tBasisType, 3)
    DO ii = 1, tsize
      obj%basisType(ii) = BaseType_ToInteger(astr(ii)%chars())
    END DO
  END IF

  DO ii = 1, 3
    astr(ii) = ""
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBasisTypeFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportAlphaFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound, isAlphaScalar
  REAL(DFP) :: alpha(3)
  INTEGER(I4B) :: tAlpha, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportAlphaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="alpha", VALUE=alpha, tsize=tAlpha, &
                 isScalar=isAlphaScalar, origin=origin, STAT=STAT, &
                 isFound=isFound)

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  IF (isAlphaScalar) THEN
    obj%alpha = alpha(1)
  ELSE
    tsize = MIN(tAlpha, 3)
    DO ii = 1, tsize
      obj%alpha(ii) = alpha(ii)
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportAlphaFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBetaFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound, isbetaScalar
  REAL(DFP) :: beta(3)
  INTEGER(I4B) :: tbeta, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBetaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="beta", VALUE=beta, tsize=tbeta, &
                 isScalar=isbetaScalar, origin=origin, STAT=STAT, &
                 isFound=isFound)

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  IF (isbetaScalar) THEN
    obj%beta = beta(1)
  ELSE
    tsize = MIN(tbeta, 3)
    DO ii = 1, tsize
      obj%beta(ii) = beta(ii)
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBetaFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportLambdaFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, STAT

  ! internal variables
  LOGICAL(LGT) :: isFound, islambdaScalar
  REAL(DFP) :: lambda(3)
  INTEGER(I4B) :: tlambda, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportlambdaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="lambda", VALUE=lambda, tsize=tlambda, &
                 isScalar=islambdaScalar, origin=origin, STAT=STAT, &
                 isFound=isFound)

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  IF (islambdaScalar) THEN
    obj%lambda = lambda(1)
  ELSE
    tsize = MIN(tlambda, 3)
    DO ii = 1, tsize
      obj%lambda(ii) = lambda(ii)
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportLambdaFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportBrokenSpaceFromToml(obj, table, origin, STAT)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin
  INTEGER(I4B), INTENT(INOUT) :: STAT
  ! internal variables
  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportBrokenSpaceFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="brokenSpace", VALUE=obj%isBrokenSpace, &
                default_value=math%no, origin=origin, STAT=STAT, &
                isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportBrokenSpaceFromToml

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
INTEGER(I4B) :: origin, STAT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              STAT=STAT)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
                  'the toml file :: cannot find ['//tomlName// &
                  "] table in config.")
#endif

CALL obj%ImportFromToml(table=node, elemType=elemType, nsd=nsd)

node => NULL()
IF (ALLOCATED(table)) DEALLOCATE (table)

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

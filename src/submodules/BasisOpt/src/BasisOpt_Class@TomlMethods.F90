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
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE StringUtility, ONLY: UpperCase
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE TomlUtility, ONLY: GetValue, GetValue_
USE BaseInterpolation_Method, ONLY: BaseType_ToChar, &
                                    BaseType_ToInteger, &
                                    InterpolationPoint_ToChar, &
                                    InterpolationPoint_ToInteger
USE FEVariable_Method, ONLY: FEVariable_ToInteger
USE ReferenceElement_Method, ONLY: ElementTopology, &
                                   XiDimension, &
                                   RefCoord_, &
                                   GetElementIndex
USE InterpolationUtility, ONLY: RefElemDomain

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif
INTEGER(I4B) :: ii, jj
LOGICAL(LGT) :: isok
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
CALL BaseInterpolationFromToml(obj, table)
CALL BaseContinuityFromToml(obj, table)
CALL IpTypeFromToml(obj, table)
CALL BaseTypeFromToml(obj, table)
CALL AlphaFromToml(obj, table)
CALL BetaFromToml(obj, table)
CALL LambdaFromToml(obj, table)
CALL FeTypeFromToml(obj, table)
CALL obj%quadOpt%ImportFromToml(table=table)

isok = PRESENT(elemType)
IF (isok) THEN
  obj%elemType = elemType
  obj%topoName = ElementTopology(elemType)
  obj%xidim = XiDimension(obj%topoName)
  astr = RefElemDomain(elemType=obj%topoName, &
                       baseContinuity=obj%baseContinuity, &
                       baseInterpol=obj%baseInterpolation)
  obj%refelemDomain = astr%Slice(1, 1)
  astr = ""
  CALL RefCoord_(elemType=obj%topoName, ans=obj%refelemCoord, &
                 nrow=ii, ncol=jj, refelem=obj%refelemDomain)
  obj%elemIndx = GetElementIndex(obj%topoName)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                         HelpImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE BaseInterpolationFromToml(obj, table)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "BaseInterpolationFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="baseInterpolation", VALUE=astr, &
                default_value=TypeBasisOpt%baseInterpolation, &
                origin=origin, stat=stat, isFound=isFound)

  obj%baseInterpolation = UpperCase(astr%slice(1, 4))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE BaseInterpolationFromToml

!----------------------------------------------------------------------------
!                                                         HelpImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE BaseContinuityFromToml(obj, table)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "BaseContinuityFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="baseContinuity", &
                VALUE=astr, &
                default_value=TypeBasisOpt%baseContinuity, &
                origin=origin, stat=stat, isFound=isFound)

  obj%baseContinuity = UpperCase(astr%slice(1, 2))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE BaseContinuityFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE IpTypeFromToml(obj, table)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "IpTypeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="ipType", VALUE=astr, &
                default_value=TypeBasisOpt%ipType_char, &
                origin=origin, stat=stat, isFound=isFound)
  obj%ipType_char = astr%chars()
  obj%ipType = InterpolationPoint_ToInteger(astr%chars())

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE IpTypeFromToml

!----------------------------------------------------------------------------
!                                                              FeTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE FeTypeFromToml(obj, table)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FeTypeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="feType", VALUE=astr, &
                default_value=TypeBasisOpt%feType_char, &
                origin=origin, stat=stat, isFound=isFound)
  obj%feType_char = astr%chars()
  obj%feType = FEVariable_ToInteger(astr%chars())

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE FeTypeFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE BaseTypeFromToml(obj, table)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound, isBasisTypeScalar
  TYPE(String) :: astr(3)
  INTEGER(I4B) :: tBasisType, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "BaseTypeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="basisType", &
                 VALUE=astr, tsize=tBasisType, &
                 isScalar=isBasisTypeScalar, &
                 origin=origin, stat=stat, isFound=isFound)
  CALL AssertError1(isFound, myName, "basisType is not found in toml")

  IF (isBasisTypeScalar) THEN
    obj%basisType = BaseType_ToInteger(astr(1)%chars())
  ELSE
    tsize = MIN(tBasisType, 3)
    DO ii = 1, tsize
      obj%basisType(ii) = BaseType_ToInteger(astr(ii)%chars())
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE BaseTypeFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE AlphaFromToml(obj, table)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound, isAlphaScalar
  REAL(DFP) :: alpha(3)
  INTEGER(I4B) :: tAlpha, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "AlphaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="alpha", VALUE=alpha, tsize=tAlpha, &
            isScalar=isAlphaScalar, origin=origin, stat=stat, isFound=isFound)

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
END SUBROUTINE AlphaFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE BetaFromToml(obj, table)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound, isbetaScalar
  REAL(DFP) :: beta(3)
  INTEGER(I4B) :: tbeta, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "BetaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="beta", VALUE=beta, tsize=tbeta, &
             isScalar=isbetaScalar, origin=origin, stat=stat, isFound=isFound)

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
END SUBROUTINE BetaFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE LambdaFromToml(obj, table)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound, islambdaScalar
  REAL(DFP) :: lambda(3)
  INTEGER(I4B) :: tlambda, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "lambdaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="lambda", VALUE=lambda, tsize=tlambda, &
           isScalar=islambdaScalar, origin=origin, stat=stat, isFound=isFound)

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
END SUBROUTINE LambdaFromToml

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

CALL obj%ImportFromToml(table=node, elemType=elemType)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

node => NULL()

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

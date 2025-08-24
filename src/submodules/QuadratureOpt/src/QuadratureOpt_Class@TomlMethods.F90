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

SUBMODULE(QuadratureOpt_Class) TomlMethods

USE Display_Method, ONLY: Display, ToString
USE FPL_Method, ONLY: Set, GetValue
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar, &
                                  QuadraturePoint_ToInteger
USE InputUtility, ONLY: Input
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE TomlUtility, ONLY: GetValue, GetValue_

USE ReferenceElement_Method, ONLY: ElementType, ElementTopology

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading isHomogeneous...')
#endif

! The following call will reset the object
CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL IsHomogeneousFromToml(obj, table)
CALL NSDFromToml(obj, table)
CALL TopoTypeFromToml(obj, table)
CALL QuadratureTypeFromToml(obj, table)
CALL OrderFromToml(obj, table)
CALL NipsFromToml(obj, table)
CALL AlphaFromToml(obj, table)
CALL BetaFromToml(obj, table)
CALL LambdaFromToml(obj, table)
IF (PRESENT(topoType)) obj%topoType = topoType
IF (PRESENT(nsd)) obj%nsd = nsd
IF (PRESENT(xidim)) obj%xidim = xidim
IF (PRESENT(refelemDomain)) obj%refelemDomain = refelemDomain
CALL obj%SetRefElemCoord(refelemCoord=refelemCoord)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                       IsHomogeneousFromToml
!----------------------------------------------------------------------------

SUBROUTINE IsHomogeneousFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "IsHomogeneousFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="isHomogeneous", VALUE=obj%isHomogeneous, &
                default_value=TypeQuadratureOpt%isHomogeneous, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE IsHomogeneousFromToml

!----------------------------------------------------------------------------
!                                                                NSDFromToml
!----------------------------------------------------------------------------

SUBROUTINE NSDFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "NSDFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="nsd", VALUE=obj%nsd, &
                default_value=TypeQuadratureOpt%nsd, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE NSDFromToml

!----------------------------------------------------------------------------
!                                                           TopoTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE TopoTypeFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "TopoTypeFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="topoType", VALUE=astr, &
                default_value="NA", &
                origin=origin, stat=stat, isFound=isFound)

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    astr = ""
    RETURN
  END IF

  ! Convert the string to integer
  obj%topoType = ElementType(astr%chars())
  obj%topoType = ElementTopology(obj%topoType)

  astr = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE TopoTypeFromToml

!----------------------------------------------------------------------------
!                                                     QuadratureTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE QuadratureTypeFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "QuadratureTypeFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, tsize, ii
  LOGICAL(LGT) :: isFound, isScalar
  TYPE(String) :: astr(3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="quadratureType", VALUE=astr, &
                 tsize=tsize, isScalar=isScalar, origin=origin, stat=stat, &
                 isFound=isFound)

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  IF (isScalar) THEN
    obj%quadratureType = QuadraturePoint_ToInteger(astr(1)%chars())
    DO ii = 1, 3
      obj%quadratureType_char(ii) = astr(1)%chars()
    END DO
  ELSE
    tsize = MIN(tsize, 3)
    DO ii = 1, tsize
      obj%quadratureType(ii) = QuadraturePoint_ToInteger(astr(ii)%chars())
      obj%quadratureType_char(ii) = astr(ii)%chars()
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE QuadratureTypeFromToml

!----------------------------------------------------------------------------
!                                                     QuadratureTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE OrderFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "OrderFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, tsize, ii, myint(3)
  LOGICAL(LGT) :: isScalar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="order", VALUE=myint, tsize=tsize, &
                 isScalar=isScalar, origin=origin, stat=stat, &
                 isFound=obj%isOrder)

  IF (.NOT. obj%isOrder) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  IF (isScalar) THEN
    obj%order = myint(1)
  ELSE
    tsize = MIN(tsize, 3)
    DO ii = 1, tsize
      obj%order(ii) = myint(ii)
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE OrderFromToml

!----------------------------------------------------------------------------
!                                                               NipsFromToml
!----------------------------------------------------------------------------

SUBROUTINE NipsFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "NipsFromToml()"
#endif

  INTEGER(I4B) :: origin, stat, tsize, ii, myint(3)
  LOGICAL(LGT) :: isScalar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="nips", VALUE=myint, tsize=tsize, &
              isScalar=isScalar, origin=origin, stat=stat, isFound=obj%isNips)

  IF (.NOT. obj%isNips) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  IF (isScalar) THEN
    obj%nips = myint(1)
  ELSE
    tsize = MIN(tsize, 3)
    DO ii = 1, tsize
      obj%nips(ii) = myint(ii)
    END DO
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE NipsFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE AlphaFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
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
END SUBROUTINE AlphaFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE BetaFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
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
END SUBROUTINE BetaFromToml

!----------------------------------------------------------------------------
!                                                             IpTypeFromToml
!----------------------------------------------------------------------------

SUBROUTINE LambdaFromToml(obj, table)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound, isScalar
  REAL(DFP) :: lambda(3)
  INTEGER(I4B) :: tlambda, ii, tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "LambdaFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue_(table=table, key="lambda", VALUE=lambda, tsize=tlambda, &
                 isScalar=isScalar, origin=origin, stat=stat, isFound=isFound)

  IF (.NOT. isFound) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  IF (isScalar) THEN
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
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Import TimeOpt from toml file

MODULE PROCEDURE obj_ImportFromToml2
! internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif
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

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node, topoType=topoType, nsd=nsd, xidim=xidim, &
                       refelemdomain=refelemDomain, refelemCoord=refelemCoord)

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

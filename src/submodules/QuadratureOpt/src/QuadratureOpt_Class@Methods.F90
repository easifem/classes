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

SUBMODULE(QuadratureOpt_Class) Methods

USE Display_Method, ONLY: Display, ToString
USE FPL_Method, ONLY: Set, GetValue
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar, &
                                  QuadraturePoint_ToInteger
USE InputUtility, ONLY: Input
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE TomlUtility, ONLY: GetValue, GetValue_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-06-27
! summary: Copy the content from obj2 to obj

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = obj2%isInit
obj%isHomogeneous = obj2%isHomogeneous
obj%topoName = obj2%topoName
obj%nsd = obj2%nsd
obj%quadratureType = obj2%quadratureType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%order = obj2%order
obj%isOrder = obj2%isOrder
obj%nips = obj2%nips
obj%isNips = obj2%isNips
obj%quadratureType_char = obj2%quadratureType_char

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
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isInit, "isInit: ", unitNo=unitNo)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%topoName, "topoName: ", unitNo=unitNo)
CALL Display(obj%nsd, "nsd: ", unitNo=unitNo)
CALL Display(obj%isOrder, "isOrder: ", unitNo=unitNo)
CALL Display(obj%isNips, "isNips: ", unitNo=unitNo)

DO ii = 1, obj%nsd
  CALL Display(obj%quadratureType(ii), &
               "quadratureType("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(TRIM(obj%quadratureType_char(ii)), &
               "quadratureType_char("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(obj%alpha(ii), "alpha("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(obj%beta(ii), "beta("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%lambda(ii), "lambda("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%order(ii), "order("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%nips(ii), "nips("//ToString(ii)//', 1): ', &
               unitNo=unitNo)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                SetQuadratureOptParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetQuadratureOptParam1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetQuadratureOptParam()"
#endif
INTEGER(I4B) :: myint
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! setting isHomogeneous
CALL Set(obj=param, prefix=prefix, key="isHomogeneous", &
         datatype=.TRUE., VALUE=.TRUE.)

! setting quadratureType
myint = Input(default=TypeQuadratureOpt%quadratureType(1), &
              option=quadratureType)
CALL Set(obj=param, prefix=prefix, key="quadratureType", &
         datatype=myint, VALUE=myint)

! setting order
abool = PRESENT(order)
CALL Set(obj=param, prefix=prefix, key="isOrder", &
         datatype=abool, VALUE=abool)
myint = Input(default=TypeQuadratureOpt%order(1), &
              option=order)
CALL Set(obj=param, prefix=prefix, key="order", &
         datatype=myint, VALUE=myint)

! setting nips
abool = PRESENT(order)
CALL Set(obj=param, prefix=prefix, key="isNips", &
         datatype=abool, VALUE=abool)

myint = Input(default=TypeQuadratureOpt%nips(1), &
              option=nips)
CALL Set(obj=param, prefix=prefix, key="nips", &
         datatype=myint, VALUE=myint)

! setting alpha
areal = Input(default=TypeQuadratureOpt%alpha(1), &
              option=alpha)
CALL Set(obj=param, prefix=prefix, key="alpha", &
         datatype=areal, VALUE=areal)

! setting beta
areal = Input(default=TypeQuadratureOpt%beta(1), &
              option=beta)
CALL Set(obj=param, prefix=prefix, key="beta", &
         datatype=areal, VALUE=areal)

! setting lambda
areal = Input(default=TypeQuadratureOpt%lambda(1), &
              option=lambda)
CALL Set(obj=param, prefix=prefix, key="lambda", &
         datatype=areal, VALUE=areal)

! setting nsd
myint = Input(default=TypeQuadratureOpt%nsd, &
              option=nsd)
CALL Set(obj=param, prefix=prefix, key="nsd", &
         datatype=myint, VALUE=myint)

! setting topoName
myint = Input(default=TypeQuadratureOpt%topoName, &
              option=topoName)
CALL Set(obj=param, prefix=prefix, key="topoName", &
         datatype=myint, VALUE=myint)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE SetQuadratureOptParam1

!----------------------------------------------------------------------------
!                                                SetQuadratureOptParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetQuadratureOptParam2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetQuadratureOptParam()"
#endif
INTEGER(I4B) :: myint(3)
REAL(DFP) :: areal(3)
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! setting isHomogeneous
CALL Set(obj=param, prefix=prefix, key="isHomogeneous", &
         datatype=.FALSE., VALUE=.FALSE.)

! setting quadratureType
myint = Input(default=TypeQuadratureOpt%quadratureType, &
              option=quadratureType)
CALL Set(obj=param, prefix=prefix, key="quadratureType", &
         datatype=myint, VALUE=myint)

! setting order
abool = PRESENT(order)
CALL Set(obj=param, prefix=prefix, key="isOrder", datatype=abool, &
         VALUE=abool)
myint = Input(default=TypeQuadratureOpt%order, option=order)
CALL Set(obj=param, prefix=prefix, key="order", datatype=myint, &
         VALUE=myint)

! setting nips
abool = PRESENT(order)
CALL Set(obj=param, prefix=prefix, key="isNips", &
         datatype=abool, VALUE=abool)

myint = Input(default=TypeQuadratureOpt%nips, &
              option=nips)
CALL Set(obj=param, prefix=prefix, key="nips", &
         datatype=myint, VALUE=myint)

! setting alpha
areal = Input(default=TypeQuadratureOpt%alpha, &
              option=alpha)
CALL Set(obj=param, prefix=prefix, key="alpha", &
         datatype=areal, VALUE=areal)

! setting beta
areal = Input(default=TypeQuadratureOpt%beta, &
              option=beta)
CALL Set(obj=param, prefix=prefix, key="beta", &
         datatype=areal, VALUE=areal)

! setting lambda
areal = Input(default=TypeQuadratureOpt%lambda, &
              option=lambda)
CALL Set(obj=param, prefix=prefix, key="lambda", &
         datatype=areal, VALUE=areal)

! setting nsd
myint(1) = Input(default=TypeQuadratureOpt%nsd, &
                 option=nsd)
CALL Set(obj=param, prefix=prefix, key="nsd", &
         datatype=myint(1), VALUE=myint(1))

! setting topoName
myint(1) = Input(default=TypeQuadratureOpt%topoName, &
                 option=topoName)
CALL Set(obj=param, prefix=prefix, key="topoName", &
         datatype=myint(1), VALUE=myint(1))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetQuadratureOptParam2

!----------------------------------------------------------------------------
!                                                                SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif
LOGICAL(LGT) :: ishomo, isok
INTEGER(I4B) :: a, b, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ishomo = Input(default=TypeQuadratureOpt%isHomogeneous, &
               option=isHomogeneous)

IF (ishomo) THEN
  a = 1; b = 1
ELSE
  a = 1; b = 3
END IF

! Set quadratureType if present
isok = PRESENT(quadratureType)
IF (isok) THEN
  DO ii = a, b
    obj%quadratureType(ii) = quadratureType(ii)
    obj%quadratureType_char(ii) = QuadraturePoint_ToChar( &
                                  obj%quadratureType(ii), &
                                  isUpper=.TRUE.)
  END DO
END IF

! Set order if present
isok = PRESENT(order)
IF (isok) THEN
  obj%isOrder = isok
  DO ii = a, b
    obj%order(ii) = order(ii)
  END DO
END IF

! Set number of integration points if present
isok = PRESENT(nips)
IF (isok) THEN
  obj%isNips = isok
  DO ii = a, b
    obj%nips(ii) = nips(ii)
  END DO
END IF

! Set alpha parameter for Jacobi polynomials if present
isok = PRESENT(alpha)
IF (isok) THEN
  DO ii = a, b
    obj%alpha(ii) = alpha(ii)
  END DO
END IF

! Set beta parameter for Jacobi polynomials if present
isok = PRESENT(beta)
IF (isok) THEN
  DO ii = a, b
    obj%beta(ii) = beta(ii)
  END DO
END IF

! Set lambda parameter for Ultraspherical polynomials if present
isok = PRESENT(lambda)
IF (isok) THEN
  DO ii = a, b
    obj%lambda(ii) = lambda(ii)
  END DO
END IF

! Set nsd
isok = PRESENT(nsd)
IF (isok) obj%nsd = nsd

! Set topoName
isok = PRESENT(topoName)
IF (isok) obj%topoName = topoName

isok = PRESENT(isOrder)
IF (isok) obj%isOrder = isOrder

isok = PRESENT(isNips)
IF (isok) obj%isNips = isNips

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
! Internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.

CALL GetValue(obj=param, prefix=prefix, key="isHomogeneous", &
              VALUE=obj%isHomogeneous)

IF (obj%isHomogeneous) THEN
  CALL InitiateFromParamHomogeneous(obj=obj, param=param, prefix=prefix)
ELSE
  CALL InitiateFromParamInHomogeneous(obj=obj, param=param, prefix=prefix)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                               InitiateFromParamHomogeneous
!----------------------------------------------------------------------------

SUBROUTINE InitiateFromParamHomogeneous(obj, param, prefix)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateFromParamHomogenous()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(obj=param, prefix=prefix, key="isHomogeneous", &
                VALUE=obj%isHomogeneous)

  CALL GetValue(obj=param, prefix=prefix, key="topoName", VALUE=obj%topoName)

  CALL GetValue(obj=param, prefix=prefix, key="nsd", VALUE=obj%nsd)

  CALL GetValue(obj=param, prefix=prefix, key="quadratureType", &
                VALUE=obj%quadratureType(1))

  CALL GetValue(obj=param, prefix=prefix, key="alpha", VALUE=obj%alpha(1))

  CALL GetValue(obj=param, prefix=prefix, key="beta", VALUE=obj%beta(1))

  CALL GetValue(obj=param, prefix=prefix, key="lambda", VALUE=obj%lambda(1))

  CALL GetValue(obj=param, prefix=prefix, key="order", VALUE=obj%order(1))

  CALL GetValue(obj=param, prefix=prefix, key="nips", VALUE=obj%nips(1))

  CALL GetValue(obj=param, prefix=prefix, key="isNips", VALUE=obj%isNips)

  CALL GetValue(obj=param, prefix=prefix, key="isOrder", VALUE=obj%isOrder)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE InitiateFromParamHomogeneous

!----------------------------------------------------------------------------
!                                               InitiateFromParamHomogeneous
!----------------------------------------------------------------------------

SUBROUTINE InitiateFromParamInHomogeneous(obj, param, prefix)
  CLASS(QuadratureOpt_), INTENT(INOUT) :: obj
  TYPE(ParameterList_), INTENT(IN) :: param
  CHARACTER(*), INTENT(IN) :: prefix

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateFromParamHomogenous()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(obj=param, prefix=prefix, key="isHomogeneous", &
                VALUE=obj%isHomogeneous)
  CALL GetValue(obj=param, prefix=prefix, key="isNips", VALUE=obj%isNips)
  CALL GetValue(obj=param, prefix=prefix, key="isOrder", VALUE=obj%isOrder)
  CALL GetValue(obj=param, prefix=prefix, key="topoName", VALUE=obj%topoName)
  CALL GetValue(obj=param, prefix=prefix, key="nsd", VALUE=obj%nsd)
  CALL GetValue(obj=param, prefix=prefix, key="quadratureType", &
                VALUE=obj%quadratureType)
  CALL GetValue(obj=param, prefix=prefix, key="alpha", VALUE=obj%alpha)
  CALL GetValue(obj=param, prefix=prefix, key="beta", VALUE=obj%beta)
  CALL GetValue(obj=param, prefix=prefix, key="lambda", VALUE=obj%lambda)
  CALL GetValue(obj=param, prefix=prefix, key="order", VALUE=obj%order)
  CALL GetValue(obj=param, prefix=prefix, key="nips", VALUE=obj%nips)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE InitiateFromParamInHomogeneous

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.

CALL obj%SetParam(isHomogeneous=isHomogeneous, &
                  quadratureType=quadratureType, &
                  order=order, nips=nips, alpha=alpha, &
                  beta=beta, lambda=lambda, nsd=nsd, &
                  topoName=topoName, isOrder=isOrder, &
                  isNips=isNips)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                   GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(isHomogeneous)) isHomogeneous = obj%isHomogeneous
IF (PRESENT(quadratureType)) quadratureType = obj%quadratureType
IF (PRESENT(order)) order = obj%order
IF (PRESENT(nips)) nips(1:3) = obj%nips(1:3)
IF (PRESENT(alpha)) alpha = obj%alpha
IF (PRESENT(beta)) beta = obj%beta
IF (PRESENT(lambda)) lambda = obj%lambda
IF (PRESENT(nsd)) nsd = obj%nsd
IF (PRESENT(topoName)) topoName = obj%topoName

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"

CHARACTER(:), ALLOCATABLE :: key
INTEGER(I4B) :: origin, stat, order(3), nips(3), tsize, &
                quadratureType(3), ii
REAL(DFP) :: alpha(3), beta(3), lambda(3)
INTEGER(I4B), ALLOCATABLE :: aintvec(:)
REAL(DFP), ALLOCATABLE :: arealvec(:)
TYPE(String), ALLOCATABLE :: quadratureType_char(:)
LOGICAL(LGT) :: isQuadratureTypeScalar, isok, isFound, isOrder, isNips, &
                isHomogeneous, isOrderScalar, isNipsScalar, &
                isAlpha, isAlphaScalar, isBeta, isBetaScalar, &
                isLambda, isLambdaScalar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading isHomogeneous...')
#endif

key = "isHomogeneous"
CALL GetValue(table=table, key=key, &
              VALUE=isHomogeneous, &
              default_value=TypeQuadratureOpt%isHomogeneous, &
              origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading quadratureType...')
#endif

CALL GetValue(table=table, key="quadratureType", &
              VALUE=quadratureType_char, &
              origin=origin, stat=stat, isFound=isFound, &
              isScalar=isQuadratureTypeScalar)

#ifdef DEBUG_VER
isok = isFound
CALL AssertError1(isok, myName, &
                  "quadratureType not found")
#endif

IF (isQuadratureTypeScalar) THEN
  quadratureType = QuadraturePoint_ToInteger(quadratureType_char(1)%chars())
ELSE
  tsize = SIZE(quadratureType_char)
  DO ii = 1, tsize
    quadratureType(ii) = QuadraturePoint_ToInteger( &
                         quadratureType_char(ii)%chars())
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading order...')
#endif

key = "order"
CALL GetValue(table=table, key=key, VALUE=aintvec, origin=origin, &
              stat=stat, isFound=isOrder, isScalar=isOrderScalar)

isok = isOrder .AND. isOrderScalar .AND. ALLOCATED(aintvec)
IF (isok) order = aintvec(1)

isok = isOrder .AND. ALLOCATED(aintvec)
IF (isok) THEN
  tsize = SIZE(aintvec)
  DO ii = 1, tsize
    order(ii) = aintvec(ii)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading nips...')
#endif

key = "nips"
CALL GetValue(table=table, key=key, VALUE=aintvec, origin=origin, &
              stat=stat, isFound=isNips, isScalar=isNipsScalar)

isok = isNips .AND. isNipsScalar .AND. ALLOCATED(aintvec)
IF (isok) nips = aintvec(1)

isok = isNips .AND. ALLOCATED(aintvec)
IF (isok) THEN
  tsize = SIZE(aintvec)
  DO ii = 1, tsize
    nips(ii) = aintvec(ii)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        'Reading alpha...')
#endif

key = "alpha"
CALL GetValue(table=table, key=key, VALUE=arealvec, origin=origin, &
              stat=stat, isFound=isAlpha, isScalar=isAlphaScalar)

isok = isAlpha .AND. isAlphaScalar .AND. ALLOCATED(arealvec)
IF (isok) alpha = arealvec(1)

isok = isalpha .AND. ALLOCATED(arealvec)
IF (isok) THEN
  tsize = SIZE(arealvec)
  DO ii = 1, tsize
    alpha(ii) = arealvec(ii)
  END DO
END IF

key = "beta"
CALL GetValue(table=table, key=key, VALUE=arealvec, origin=origin, &
              stat=stat, isFound=isBeta, isScalar=isBetaScalar)

isok = isBeta .AND. isBetaScalar .AND. ALLOCATED(arealvec)
IF (isok) beta = arealvec(1)

isok = isBeta .AND. ALLOCATED(arealvec)
IF (isok) THEN
  tsize = SIZE(arealvec)
  DO ii = 1, tsize
    beta(ii) = arealvec(ii)
  END DO
END IF

key = "lambda"
CALL GetValue(table=table, key=key, VALUE=arealvec, origin=origin, &
              stat=stat, isFound=isLambda, isScalar=isLambdaScalar)

isok = isLambda .AND. isLambdaScalar .AND. ALLOCATED(arealvec)
IF (isok) lambda = arealvec(1)

isok = isLambda .AND. ALLOCATED(arealvec)
IF (isok) THEN
  tsize = SIZE(arealvec)
  DO ii = 1, tsize
    lambda(ii) = arealvec(ii)
  END DO
END IF

! Here we call initiate methods with above parameters

CALL obj%Initiate(quadratureType=quadratureType, &
                  order=order, isOrder=isOrder, isOrderScalar=isOrderScalar, &
                  nips=nips, isNips=isNips, isNipsScalar=isNipsScalar, &
                  alpha=alpha, isAlpha=isAlpha, isAlphaScalar=isAlphaScalar, &
                  beta=beta, isBeta=isBeta, isBetaScalar=isBetaScalar, &
              lambda=lambda, isLambda=isLambda, isLambdaScalar=isLambdaScalar)

IF (ALLOCATED(aintvec)) DEALLOCATE (aintvec)
IF (ALLOCATED(arealvec)) DEALLOCATE (arealvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

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
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL obj%Copy(TypeQuadratureOpt)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods

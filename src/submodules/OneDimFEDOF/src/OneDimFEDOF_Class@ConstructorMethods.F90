! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(OneDimFEDOF_Class) ConstructorMethods
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString
USE IntVector_Method, ONLY: IntegerCopy => Copy
USE StringUtility, ONLY: UpperCase
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: TypeInterpolationOpt, TypePolynomialOpt
USE LagrangeOneDimFE_Class, ONLY: LagrangeOneDimFEPointer
USE HierarchicalOneDimFE_Class, ONLY: HierarchicalOneDimFEPointer

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif
INTEGER(I4B) :: order0(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order0 = order

CALL obj%Initiate2( &
  order=order0, mesh=mesh, baseContinuity=baseContinuity, &
  baseInterpolation=baseInterpolation, fetype=fetype, ipType=ipType, &
  basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, &
  quadratureType=quadratureType, quadratureOrder=quadratureOrder, &
  quadratureNips=quadratureNips, quadratureAlpha=quadratureAlpha, &
  quadratureBeta=quadratureBeta, quadratureLambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tcells, ii
CHARACTER(LEN=4) :: baseInterpolation0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%mesh => mesh

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%mesh)
CALL AssertError1(isok, myName, "mesh is not associated")
#endif

baseInterpolation0 = UpperCase(baseInterpolation(1:4))

SELECT CASE (baseInterpolation0)
CASE ("LAGR")
  obj%fe => LagrangeOneDimFEPointer()
CASE ("HIER", "HEIR")
  obj%fe => HierarchicalOneDimFEPointer()

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for baseInterpolation')
#endif

END SELECT

CALL obj%fe%Initiate( &
  baseContinuity=baseContinuity, baseInterpolation=baseInterpolation, &
  feType=feType, ipType=ipType, basisType=basisType, alpha=alpha, &
  beta=beta, lambda=lambda, quadratureType=quadratureType, &
  quadratureOrder=quadratureOrder, quadratureNips=quadratureNips, &
  quadratureAlpha=quadratureAlpha, quadratureBeta=quadratureBeta, &
  quadratureLambda=quadratureLambda)

tcells = obj%mesh%GetTotalElements()
CALL Reallocate(obj%cellOrder, tcells)
CALL Reallocate(obj%cellIA, tcells + 1)
CALL obj%SetCellOrder(order=order, islocal=islocal)

obj%tdof = obj%mesh%GetTotalVertexNodes()
DO ii = 1, tcells
  obj%tdof = obj%tdof + obj%cellOrder(ii) - 1
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B), ALLOCATABLE :: order0(:)
INTEGER(I4B) :: telems, tsize, globalElement, localElement, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

telems = mesh%GetTotalElements()
tsize = SIZE(order, 2)

#ifdef DEBUG_VER
ii = SIZE(order, 1)
isok = ii .EQ. 2
CALL AssertError1(isok, myName, &
  'number of rows of order array is not equal to 2 '//'found: '//ToString(ii))

isok = tsize .EQ. telems
CALL AssertError1(isok, myName, &
           'number of cols of order array is not equal to number of elements')
#endif

ALLOCATE (order0(telems))

DO ii = 1, telems
  globalElement = order(1, ii)
  localElement = mesh%GetLocalElemNumber(globalElement=globalElement, &
                                         islocal=.FALSE.)
  order0(localElement) = order(2, ii)
END DO

CALL obj%Initiate( &
  mesh=mesh, baseContinuity=baseContinuity, &
  baseInterpolation=baseInterpolation, order=order0, ipType=ipType, &
  basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, islocal=.TRUE.)

DEALLOCATE (order0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%tdof = 0
obj%maxTotalConnectivity = 0
obj%maxCellOrder = 0
obj%mesh => NULL()
IF (ALLOCATED(obj%cellOrder)) DEALLOCATE (obj%cellOrder)
IF (ALLOCATED(obj%cellIA)) DEALLOCATE (obj%cellIA)

IF (ASSOCIATED(obj%fe)) THEN
  CALL obj%fe%DEALLOCATE()
END IF
obj%fe => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = obj2%isInit
obj%tdof = obj2%tdof
obj%maxTotalConnectivity = obj2%maxTotalConnectivity
obj%maxCellOrder = obj2%maxCellOrder
isok = ALLOCATED(obj2%cellOrder)
IF (isok) THEN
  CALL IntegerCopy(x=obj%cellOrder, y=obj2%cellOrder)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                               IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods

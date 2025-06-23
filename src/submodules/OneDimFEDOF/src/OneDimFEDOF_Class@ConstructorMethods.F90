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

USE FPL_Method, ONLY: Set, GetValue, CheckEssentialParam

USE BaseType, ONLY: TypeInterpolationOpt, &
                    TypePolynomialOpt

USE LagrangeOneDimFE_Class, ONLY: LagrangeOneDimFEPointer

USE HierarchicalOneDimFE_Class, ONLY: HierarchicalOneDimFEPointer

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CALL CheckEssentialParam(obj=param, keys=essentialParam, prefix=myprefix, &
                         myName=myName, modName=modName)
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                        SetOneDimFEDOFParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetOneDimFEDOFParam
CHARACTER(*), PARAMETER :: myName = "SetOneDimFEDOFParam()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

! INTEGER(I4B) :: AINT(3)
! INTEGER(I4B), PARAMETER :: ipType0 = TypeInterpolationOpt%Equidistance
! INTEGER(I4B), PARAMETER :: basisType0(3) = TypePolynomialOpt%Monomial
! REAL(DFP) :: areal(3)
! REAL(DFP), PARAMETER :: three_zero(3) = [0.0, 0.0, 0.0]
! REAL(DFP), PARAMETER :: three_half(3) = [0.5, 0.5, 0.5]
!
! CALL Set(obj=param, prefix=myprefix, key="baseContinuity", &
!          VALUE=baseContinuity, dataType=baseContinuity)
!
! CALL Set(obj=param, prefix=myprefix, key="baseInterpolation", &
!          VALUE=baseInterpolation, dataType=baseInterpolation)
!
! CALL Set(obj=param, prefix=myprefix, key="orderFile", &
!          VALUE=orderFile, dataType=orderFile)
!
! CALL Set(obj=param, prefix=myprefix, key="ipType", &
!          VALUE=Input(option=ipType, default=ipType0), &
!          dataType=ipType0)
!
! CALL make_a_int(a=basistype, a0=basistype0)
! CALL set(obj=param, prefix=myprefix, key="basistype", &
!          VALUE=aint, datatype=aint)
!
! CALL make_a_real(a=alpha, a0=three_zero)
! CALL set(obj=param, prefix=myprefix, key="alpha", &
!          VALUE=areal, datatype=areal)
!
! CALL make_a_real(a=beta, a0=three_zero)
! CALL set(obj=param, prefix=myprefix, key="beta", &
!          VALUE=areal, datatype=areal)
!
! CALL make_a_real(a=lambda, a0=three_half)
! CALL set(obj=param, prefix=myprefix, key="lambda", &
!          VALUE=areal, datatype=areal)
!
! CONTAINS
! SUBROUTINE make_a_int(a, a0)
!   INTEGER(I4B), OPTIONAL, INTENT(IN) :: a(:)
!   INTEGER(I4B), INTENT(IN) :: a0(:)
!   LOGICAL(LGT) :: isok
!
!   isok = PRESENT(a)
!   IF (isok) THEN
!     SELECT CASE (SIZE(a))
!     CASE (1)
!       aint = a(1)
!     CASE (2)
!       AINT(1:2) = a(1:2)
!     CASE (3)
!       aint = a
!     CASE DEFAULT
!       aint = a(1:3)
!     END SELECT
!   ELSE
!     aint = a0
!   END IF
!
! END SUBROUTINE make_a_int
!
! SUBROUTINE make_a_real(a, a0)
!   REAL(DFP), OPTIONAL, INTENT(IN) :: a(:)
!   REAL(DFP), INTENT(IN) :: a0(:)
!   LOGICAL(LGT) :: isok
!
!   isok = PRESENT(a)
!   IF (isok) THEN
!     SELECT CASE (SIZE(a))
!     CASE (1)
!       areal = a(1)
!     CASE (2)
!       areal(1:2) = a(1:2)
!     CASE (3)
!       areal = a
!     CASE DEFAULT
!       areal = a(1:3)
!     END SELECT
!   ELSE
!     areal = a0
!   END IF
!
! END SUBROUTINE

END PROCEDURE SetOneDimFEDOFParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
INTEGER(I4B) :: order0(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order0 = order

CALL obj%Initiate2(order=order0, mesh=mesh, baseContinuity=baseContinuity, &
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
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
INTEGER(I4B) :: tcells, ii
CHARACTER(LEN=4) :: baseInterpolation0
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%mesh => mesh

isok = ASSOCIATED(obj%mesh)
CALL AssertError1(isok, myName, "mesh is not associated")

baseInterpolation0 = UpperCase(baseInterpolation(1:4))

SELECT CASE (baseInterpolation0)
CASE ("LAGR")
  obj%fe => LagrangeOneDimFEPointer()
CASE ("HIER", "HEIR")
  obj%fe => HierarchicalOneDimFEPointer()
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for baseInterpolation')
  RETURN
END SELECT

CALL obj%fe%Initiate(baseContinuity=baseContinuity, &
                     baseInterpolation=baseInterpolation, &
                     feType=feType, ipType=ipType, &
                     basisType=basisType, alpha=alpha, &
                     beta=beta, lambda=lambda, &
                     quadratureType=quadratureType, &
                     quadratureOrder=quadratureOrder, &
                     quadratureNips=quadratureNips, &
                     quadratureAlpha=quadratureAlpha, &
                     quadratureBeta=quadratureBeta, &
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
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
CHARACTER(*), PARAMETER :: myName = "obj_Initiate4()"
INTEGER(I4B), ALLOCATABLE :: order0(:)
INTEGER(I4B) :: telems, tsize, globalElement, localElement, ii
LOGICAL(LGT) :: isok
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

CALL obj%Initiate(mesh=mesh, baseContinuity=baseContinuity, &
                  baseInterpolation=baseInterpolation, order=order0, &
                  ipType=ipType, basisType=basisType, alpha=alpha, &
                  beta=beta, lambda=lambda, islocal=.TRUE.)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"

INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

CALL obj%DEALLOCATE()
obj%isInit = obj2%isInit
obj%tdof = obj2%tdof
obj%maxTotalConnectivity = obj2%maxTotalConnectivity
obj%maxCellOrder = obj2%maxCellOrder
isok = ALLOCATED(obj2%cellOrder)
IF (isok) THEN
  CALL IntegerCopy(x=obj%cellOrder, y=obj2%cellOrder)
END IF
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

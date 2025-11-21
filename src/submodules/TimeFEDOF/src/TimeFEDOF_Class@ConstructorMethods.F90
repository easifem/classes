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

SUBMODULE(TimeFEDOF_Class) ConstructorMethods
USE FEFactoryUtility, ONLY: OneDimFEFactory
USE StringUtility, ONLY: UpperCase

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%baseInterpolation = UpperCase(baseInterpolation(1:4))
IF (obj%baseInterpolation == "LAGR") obj%isLagrange = .TRUE.

#ifdef DEBUG_VER
IF (obj%isLagrange) THEN
  isok = PRESENT(ipType)
  CALL AssertError1(isok, myName, "ipType should be present")
END IF
#endif

obj%baseContinuity = UpperCase(baseContinuity(1:2))

obj%opt => timeOpt

obj%fe => OneDimFEFactory(baseContinuity=obj%baseContinuity, &
                          baseInterpolation=obj%baseInterpolation)

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myName, "obj%fe is not associated")
#endif

CALL obj%fe%Initiate( &
  baseContinuity=obj%baseContinuity, order=order, &
  baseInterpolation=obj%baseInterpolation, ipType=ipType, &
  basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, &
  feType=feType, dofType=dofType, transformType=transformType, &
  quadratureType=quadratureType, quadratureOrder=quadratureOrder, &
  quadratureIsOrder=quadratureIsOrder, quadratureNips=quadratureNips, &
  quadratureIsNips=quadratureIsNips, quadratureAlpha=quadratureAlpha, &
  quadratureBeta=quadratureBeta, quadratureLambda=quadratureLambda)

obj%cellOrder = INT(order, kind=INT8)
obj%tdof = order + 1

isok = PRESENT(scaleForQuadOrder)
IF (isok) obj%scaleForQuadOrder = INT(scaleForQuadOrder, kind=INT8)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%isLagrange = .FALSE.
obj%isMaxConSet = .FALSE.
obj%isMaxQuadPointSet = .FALSE.
obj%tdof = 0
obj%maxCon = 0
obj%maxQuadPoint = 0
obj%baseContinuity = "H1"
obj%baseInterpolation = "LAGR"
obj%scaleForQuadOrder = 2_INT8
obj%cellOrder = 0

obj%opt => NULL()

isok = ASSOCIATED(obj%fe)
IF (isok) CALL obj%fe%DEALLOCATE()
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = obj2%isInit
obj%isLagrange = obj2%isLagrange
obj%isMaxConSet = obj2%isMaxConSet
obj%isMaxQuadPointSet = obj2%isMaxQuadPointSet
obj%tdof = obj2%tdof
obj%maxCon = obj2%maxCon
obj%maxQuadPoint = obj2%maxQuadPoint
obj%baseContinuity = obj2%baseContinuity
obj%baseInterpolation = obj2%baseInterpolation
obj%scaleForQuadOrder = obj2%scaleForQuadOrder
obj%cellOrder = obj2%cellOrder
obj%opt => obj2%opt
obj%fe => obj2%fe

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods

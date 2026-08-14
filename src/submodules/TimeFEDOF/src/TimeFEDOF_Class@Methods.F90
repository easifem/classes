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

SUBMODULE(TimeFEDOF_Class) Methods
USE FEFactoryUtility, ONLY: OneDimFEFactory
USE StringUtility, ONLY: UpperCase
USE Display_Method, ONLY: Display

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
obj%opt => timeOpt

obj%baseInterpolation = UpperCase(baseInterpolation(1:4))
IF (obj%baseInterpolation == "LAGR") obj%isLagrange = .TRUE.

#ifdef DEBUG_VER
IF (obj%isLagrange) THEN
  isok = PRESENT(ipType)
  CALL AssertError1(isok, myName, "ipType should be present")
END IF
#endif

obj%baseContinuity = UpperCase(baseContinuity(1:2))

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
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInitiated: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL Display(obj%isLagrange, "isLagrange: ", unitno=unitno)
CALL Display(obj%isMaxConSet, "isMaxConSet: ", unitno=unitno)
CALL Display(obj%isMaxQuadPointSet, "isMaxQuadPointSet: ", unitno=unitno)
CALL Display(obj%tdof, "tdof: ", unitno=unitno)
CALL Display(obj%maxCon, "maxCon: ", unitno=unitno)
CALL Display(obj%maxQuadPoint, "maxQuadPoint: ", unitno=unitno)
CALL Display(obj%baseContinuity, "baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation, "baseInterpolation: ", unitno=unitno)
CALL Display(obj%scaleForQuadOrder, "scaleForQuadOrder: ", unitno=unitno)
CALL Display(obj%cellOrder, "cellOrder: ", unitno=unitno)

isok = ASSOCIATED(obj%opt)
CALL Display(isok, "opt ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%opt%Display(msg="opt: ", unitno=unitno)
END IF

isok = ASSOCIATED(obj%fe)
CALL Display(isok, "fe ASSOCIATED: ", unitno=unitno)
IF (isok) THEN
  CALL obj%fe%Display(msg="fe: ", unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                               IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                               GetCaseName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCaseName
ans = obj%baseContinuity//obj%baseInterpolation
END PROCEDURE obj_GetCaseName

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDOF

!----------------------------------------------------------------------------
!                                                           GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeOptPointer
ans => obj%opt
END PROCEDURE obj_GetTimeOptPointer

!----------------------------------------------------------------------------
!                                                       GetBaseInterpolation
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
!                                                           GetBaseContinuity
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
!                                                               GetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCellOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%cellOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetCellOrder

!----------------------------------------------------------------------------
!                                                              GetFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%fe

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFEPointer

!----------------------------------------------------------------------------
!                                                    GetMaxTotalConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalConnectivity()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isMaxConSet) THEN
  obj%maxCon = obj%tdof
  obj%isMaxConSet = .TRUE.
END IF

ans = obj%maxCon

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalConnectivity

!----------------------------------------------------------------------------
!                                                GetMaxTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalQuadraturePoints()"
#endif

INTEGER(I4B) :: cellOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isMaxQuadPointSet) THEN
  cellOrder = obj%cellOrder * obj%scaleForQuadOrder
  CALL obj%fe%SetQuadratureOrder(order=cellOrder)
  obj%maxQuadPoint = obj%fe%GetTotalQuadraturePoints()
  obj%isMaxQuadPointSet = .TRUE.
END IF

ans = obj%maxQuadPoint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                                                      SetFE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFE()"
#endif

INTEGER(I4B) :: cellOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cellOrder = obj%cellOrder
CALL obj%fe%SetOrder(order=cellOrder)
! CALL obj%fe%SetOrientation(cellOrient=cellOrient)
cellOrder = cellOrder * obj%scaleForQuadOrder
CALL obj%fe%SetQuadratureOrder(order=cellOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods

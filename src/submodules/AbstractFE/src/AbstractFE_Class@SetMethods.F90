! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

SUBMODULE(AbstractFE_Class) SetMethods

USE LagrangePolynomialUtility, ONLY: LagrangeDOF

USE HierarchicalPolynomialUtility, ONLY: HierarchicalDOF

USE ReallocateUtility, ONLY: Reallocate

USE Display_Method, ONLY: ToString, Display

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(nsd)) obj%nsd = nsd
IF (PRESENT(order)) obj%order = order
IF (PRESENT(anisoOrder)) obj%anisoOrder = anisoOrder
IF (PRESENT(edgeOrder)) obj%edgeOrder(1:SIZE(edgeOrder)) = edgeOrder
IF (PRESENT(faceOrder)) obj%faceOrder(1:3, 1:SIZE(faceOrder)) = faceOrder(1:3, :)
IF (PRESENT(cellOrder)) obj%cellOrder(1:SIZE(cellOrder)) = cellOrder
IF (PRESENT(feType)) obj%feType = feType
IF (PRESENT(elemType)) obj%elemType = elemType
IF (PRESENT(ipType)) obj%ipType = ipType
IF (PRESENT(dofType)) obj%dofType = dofType
IF (PRESENT(transformType)) obj%transformType = transformType

IF (PRESENT(baseContinuity)) obj%baseContinuity = baseContinuity(1:2)
IF (PRESENT(baseInterpolation)) obj%baseInterpolation = baseInterpolation(1:4)
IF (PRESENT(refElemDomain)) obj%refElemDomain = refElemDomain(1:1)

IF (PRESENT(isIsotropicOrder)) obj%isIsotropicOrder = isIsotropicOrder
IF (PRESENT(isAnisotropicOrder)) obj%isAnisotropicOrder = isAnisotropicOrder
IF (PRESENT(isEdgeOrder)) obj%isEdgeOrder = isEdgeOrder
IF (PRESENT(isFaceOrder)) obj%isFaceOrder = isFaceOrder
IF (PRESENT(isCellOrder)) obj%isCellOrder = isCellOrder

IF (PRESENT(tEdgeOrder)) obj%tEdgeOrder = tEdgeOrder
IF (PRESENT(tFaceOrder)) obj%tFaceOrder = tFaceOrder
IF (PRESENT(tCellOrder)) obj%tCellOrder = tCellOrder

IF (PRESENT(basisType)) obj%basisType = basisType
IF (PRESENT(alpha)) obj%alpha = alpha
IF (PRESENT(beta)) obj%beta = beta
IF (PRESENT(lambda)) obj%lambda = lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%baseInterpolation)
CASE ("LAGR")
  CALL obj%SetLagrangeOrder(order=order, anisoorder=anisoorder, &
                            errCheck=errCheck)

CASE ("HIER", "HEIR")
  CALL obj%SetHierarchicalOrder(cellOrder=cellOrder, faceOrder=faceOrder, &
          edgeOrder=edgeOrder, cellOrient=cellOrient, faceOrient=faceOrient, &
                      edgeOrient=edgeOrient, errCheck=errCheck, tcell=tcell, &
                                tface=tface, tedge=tedge)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    '[INTERNAL ERROR] :: no case found for baseInterpolation is not defined.')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                          SetLagrangeOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLagrangeOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetLagrangeOrder()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B), PARAMETER :: default_anisoOrder(3) = 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isIsotropicOrder = PRESENT(order)

IF (obj%isIsotropicOrder) THEN
  obj%order = order
  obj%tdof = LagrangeDOF(order=obj%order, elemType=obj%elemType)

#ifdef DEBUG_VER
  isok = obj%order .GT. 0
  CALL AssertError1(isok, myname, "zero order found")

  isok = obj%tdof .GT. 0
  CALL AssertError1(isok, myname, "zero tdof found")
#endif

  CALL Reallocate(obj%coeff, obj%tdof, obj%tdof, isExpand=.TRUE., &
                  expandFactor=2_I4B)
  CALL Reallocate(obj%xij, 3, obj%tdof, isExpand=.TRUE., expandFactor=2_I4B)
END IF

obj%isAnisotropicOrder = PRESENT(anisoOrder)
IF (obj%isAnisotropicOrder) THEN
  CALL obj_SetIntegerType(a=obj%anisoOrder, default_a=default_anisoOrder, &
                          b=anisoOrder, n=obj%xidim)

  obj%tdof = LagrangeDOF(p=obj%anisoOrder(1), q=obj%anisoOrder(2), &
                         r=obj%anisoOrder(3), elemType=obj%elemType)

  CALL Reallocate(obj%coeff, obj%tdof, obj%tdof, isExpand=.TRUE., &
                  expandFactor=2_I4B)
  CALL Reallocate(obj%xij, 3, obj%tdof, isExpand=.TRUE., expandFactor=2_I4B)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetLagrangeOrder

!----------------------------------------------------------------------------
!                                                       SetHierarchicalOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetHierarchicalOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetHierarchicalOrder()"
LOGICAL(LGT) :: errCheck0
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
errCheck0 = .FALSE.

IF (errCheck) errCheck0 = errCheck

IF (errCheck0) THEN
  CALL checkerror
END IF

#endif

obj%tdof = HierarchicalDOF(elemType=obj%elemType, cellOrder=cellOrder, &
                           faceOrder=faceOrder, edgeOrder=edgeOrder)

IF (PRESENT(cellOrder)) THEN

  obj%isCellOrder = .TRUE.
  obj%tCellOrder = tcell
  DO ii = 1, obj%tCellOrder
    obj%cellOrder(ii) = cellOrder(ii)
    obj%cellOrient(ii) = cellOrient(ii)
  END DO

END IF

IF (PRESENT(faceOrder)) THEN

  IF (obj%xidim .GE. 2) THEN

    obj%isFaceOrder = .TRUE.
    obj%tFaceOrder = tface

    DO ii = 1, obj%tFaceOrder
      obj%faceOrder(1:3, ii) = faceOrder(1:3, ii)
      obj%faceOrient(1:3, ii) = faceOrient(1:3, ii)
    END DO

  END IF

END IF

IF (PRESENT(edgeOrder)) THEN

  IF (obj%xidim .GE. 3) THEN

    obj%isEdgeOrder = .TRUE.
    obj%tEdgeOrder = tedge
    DO ii = 1, obj%tEdgeOrder
      obj%edgeOrder(ii) = edgeOrder(ii)
      obj%edgeOrient(ii) = edgeOrient(ii)
    END DO

  END IF

END IF

#ifdef DEBUG_VER

CONTAINS

SUBROUTINE checkerror

  LOGICAL(LGT) :: isok, abool

  isok = PRESENT(cellOrder)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: cellOrder is not present.')
    RETURN
  END IF

  isok = PRESENT(cellOrient)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: cellOrient is not present.')
    RETURN
  END IF

  isok = PRESENT(tcell)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: tcell is not present.')
    RETURN
  END IF

  isok = tcell .LE. SIZE(cellOrder)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: size of cellOrder is not enough.')
    RETURN
  END IF

  isok = tcell .LE. SIZE(cellOrient)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: size of cellOrient is not enough.')
    RETURN
  END IF

  abool = obj%xidim .GE. 2
  IF (abool) THEN

    isok = PRESENT(faceOrder)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: faceOrder is not present.')
      RETURN
    END IF

    isok = PRESENT(tface)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: tface is not present.')
      RETURN
    END IF

    isok = SIZE(faceOrder, 1) .EQ. 3

    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: rowsize in faceOrder should be 3.')
      RETURN
    END IF

    isok = SIZE(faceOrder, 2) .GE. tface

    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                       '[INTERNAL ERROR] :: colsize in faceOrder not enough.')
      RETURN
    END IF

    isok = PRESENT(faceOrient)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: faceOrient is not present.')
      RETURN
    END IF

    isok = SIZE(faceOrient, 1) .EQ. 3
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                     '[INTERNAL ERROR] :: rowsize in faceOrient should be 3.')
      RETURN
    END IF

    isok = SIZE(faceOrder, 2) .GE. tface

    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: colsize in faceorient not enough.')
      RETURN
    END IF

  END IF

  abool = obj%xidim .GE. 3
  IF (abool) THEN

    isok = PRESENT(edgeOrder)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: edgeOrder is not present.')
      RETURN
    END IF

    isok = PRESENT(edgeOrient)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: edgeOrient is not present.')
      RETURN
    END IF

    isok = PRESENT(tedge)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: tedge is not present.')
      RETURN
    END IF

    isok = SIZE(edgeOrder) .GE. tedge
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                       '[INTERNAL ERROR] :: size of edgeOrder is not enough.')
      RETURN
    END IF

    isok = SIZE(edgeOrient) .GE. tedge
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: size of edgeOrient is not enough.')
      RETURN
    END IF

  END IF

END SUBROUTINE checkerror

#endif

END PROCEDURE obj_SetHierarchicalOrder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

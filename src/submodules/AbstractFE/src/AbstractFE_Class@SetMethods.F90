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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (PRESENT(nsd)) obj%nsd = nsd
IF (PRESENT(order)) obj%order = order
IF (PRESENT(anisoOrder)) obj%anisoOrder = anisoOrder
IF (PRESENT(edgeOrder)) obj%edgeOrder(1:SIZE(edgeOrder)) = edgeOrder
IF (PRESENT(faceOrder)) obj%faceOrder(1:SIZE(faceOrder)) = faceOrder
IF (PRESENT(cellOrder)) obj%cellOrder(1:SIZE(cellOrder)) = cellOrder
IF (PRESENT(feType)) obj%feType = feType
IF (PRESENT(elemType)) obj%elemType = elemType
IF (PRESENT(ipType)) obj%ipType = ipType
IF (PRESENT(dofType)) obj%dofType = dofType
IF (PRESENT(transformType)) obj%transformType = transformType
IF (PRESENT(baseContinuity)) THEN
  CALL BaseContinuity_fromString( &
    & obj=obj%baseContinuity, &
    & name=baseContinuity)
  obj%baseContinuity0 = baseContinuity
END IF
IF (PRESENT(baseInterpolation)) THEN
  CALL BaseInterpolation_fromString( &
    & obj=obj%baseInterpolation,   &
    & name=baseInterpolation)
  obj%baseInterpolation0 = baseInterpolation
END IF
IF (PRESENT(refElemDomain)) obj%refElemDomain = refElemDomain
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
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods

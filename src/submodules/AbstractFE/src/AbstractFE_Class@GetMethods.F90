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

SUBMODULE(AbstractFE_Class) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

IF (PRESENT(nsd)) nsd = obj%nsd
IF (PRESENT(order)) order = obj%order
IF (PRESENT(anisoOrder)) anisoOrder = obj%anisoOrder

IF (PRESENT(edgeOrder)) THEN
  CALL Reallocate(edgeOrder, obj%tEdgeOrder)
  DO ii = 1, obj%tEdgeOrder
    edgeOrder(ii) = obj%edgeOrder(ii)
  END DO
END IF

IF (PRESENT(faceOrder)) THEN
  CALL Reallocate(faceOrder, obj%tfaceOrder)
  DO ii = 1, obj%tfaceOrder
    faceOrder(ii) = obj%faceOrder(ii)
  END DO
END IF

IF (PRESENT(cellOrder)) THEN
  CALL Reallocate(cellOrder, obj%tcellOrder)
  DO ii = 1, obj%tcellOrder
    cellOrder(ii) = obj%cellOrder(ii)
  END DO
END IF

IF (PRESENT(feType)) feType = obj%feType
IF (PRESENT(elemType)) elemType = obj%elemType
IF (PRESENT(ipType)) ipType = obj%ipType

IF (PRESENT(dofType)) dofType = obj%dofType
IF (PRESENT(transformType)) transformType = obj%transformType

IF (PRESENT(baseContinuity)) THEN
  baseContinuity = obj%baseContinuity
END IF

IF (PRESENT(baseInterpolation)) THEN
  baseInterpolation = obj%baseInterpolation
END IF

IF (PRESENT(refElemDomain)) refElemDomain = obj%refElemDomain
IF (PRESENT(isIsotropicOrder)) isIsotropicOrder = obj%isIsotropicOrder
IF (PRESENT(isAnisotropicOrder)) isAnisotropicOrder = obj%isAnisotropicOrder
IF (PRESENT(isEdgeOrder)) isEdgeOrder = obj%isEdgeOrder
IF (PRESENT(isFaceOrder)) isFaceOrder = obj%isFaceOrder
IF (PRESENT(isCellOrder)) isCellOrder = obj%isCellOrder

IF (PRESENT(tEdgeOrder)) tEdgeOrder = obj%tEdgeOrder
IF (PRESENT(tFaceOrder)) tFaceOrder = obj%tFaceOrder
IF (PRESENT(tCellOrder)) tCellOrder = obj%tCellOrder

IF (PRESENT(basisType)) basisType = obj%basisType
IF (PRESENT(alpha)) alpha = obj%alpha
IF (PRESENT(beta)) beta = obj%beta
IF (PRESENT(lambda)) lambda = obj%lambda

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                            GetTopologyType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTopologyType
ans = obj%topoType
END PROCEDURE obj_GetTopologyType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods

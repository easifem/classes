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
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString

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

CALL obj%opt%SetParam(nsd=nsd, order=order, anisoOrder=anisoOrder, &
              edgeOrder=edgeOrder, faceOrder=faceOrder, cellOrder=cellOrder, &
     fetype=fetype, elemType=elemType, topoType=topoType, elemIndx=elemIndx, &
                 ipType=ipType, basisType=basisType, alpha=alpha, beta=beta, &
                lambda=lambda, dofType=dofType, transformType=transformType, &
                 refElemDomain=refElemDomain, baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
   isIsotropicOrder=isIsotropicOrder, isAnisotropicOrder=isAnisotropicOrder, &
  isEdgeOrder=isEdgeOrder, isFaceOrder=isFaceOrder, isCellOrder=isCellOrder, &
          tEdgeOrder=tEdgeOrder, tFaceOrder=tFaceOrder, tCellOrder=tCellOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

INTEGER(I4B) :: tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%SetOrder(order=order, anisoOrder=anisoOrder, &
              cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
        cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient, &
                     errCheck=errCheck, tcell=tcell, tface=tface, tedge=tedge)

tdof = obj%opt%GetTotalDOF()

CALL Reallocate(obj%coeff, tdof, tdof, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL Reallocate(obj%xij, 3, tdof, isExpand=.TRUE., expandFactor=2_I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

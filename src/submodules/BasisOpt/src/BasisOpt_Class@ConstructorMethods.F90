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

SUBMODULE(BasisOpt_Class) ConstructorMethods
! USE FPL_Method, ONLY: GetValue, CheckEssentialParam, Set
USE StringUtility, ONLY: UpperCase
USE InterpolationUtility, ONLY: RefElemDomain
USE ReferenceElement_Method, ONLY: ElementTopology, &
                                   XiDimension, &
                                   RefCoord_, &
                                   GetElementIndex
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Copy => Copy
USE Display_Method, ONLY: ToString, Display
USE BasisOptUtility, ONLY: SetIntegerType, SetRealType

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

TYPE(String) :: mystr
INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%elemType = elemType
obj%nsd = nsd
obj%baseInterpolation = UpperCase(baseInterpolation(1:4))
obj%baseContinuity = UpperCase(baseContinuity(1:2))
obj%topoType = ElementTopology(elemType)
obj%fetype = fetype
obj%xidim = XiDimension(obj%topoType)
mystr = RefElemDomain(elemType=obj%topoType, &
                      baseContinuity=obj%baseContinuity, &
                      baseInterpol=obj%baseInterpolation)
obj%refelemDomain = mystr%Slice(1, 1)
mystr = ""
CALL RefCoord_(elemType=obj%topoType, ans=obj%refelemCoord, &
               nrow=ii, ncol=jj, refelem=obj%refelemDomain)

obj%isIpType = PRESENT(ipType)
IF (obj%isIpType) obj%ipType = ipType

! If b is present, then it will be set to a, otherwise nothing
CALL SetIntegerType(a=obj%basisType, b=basisType, n=obj%xidim)
CALL SetRealType(a=obj%alpha, b=alpha, n=obj%xidim)
CALL SetRealType(a=obj%beta, b=beta, n=obj%xidim)
CALL SetRealType(a=obj%lambda, b=lambda, n=obj%xidim)

obj%elemIndx = GetElementIndex(obj%topoType)

CALL obj%SetOrder( &
  order=order, anisoOrder=anisoOrder, cellOrder=cellOrder, &
  faceOrder=faceOrder, edgeOrder=edgeOrder, cellOrient=cellOrient, &
  faceOrient=faceOrient, edgeOrient=edgeOrient, tcell=tcell, &
  tface=tface, tedge=tedge, errCheck=errCheck)

CALL obj%quadOpt%Initiate(isHomogeneous=quadratureIsHomogeneous, &
                          quadratureType=quadratureType, &
                          order=quadratureOrder, &
                          isOrder=quadratureIsOrder, &
                          nips=quadratureNips, &
                          isNips=quadratureIsNips, &
                          alpha=quadratureAlpha, &
                          beta=quadratureBeta, &
                          lambda=quadratureLambda, &
                          topoType=obj%topoType, &
                          nsd=obj%nsd, &
                          xidim=obj%xidim, &
                          refelemDomain=obj%refelemDomain, &
                          refelemCoord=obj%refelemCoord)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                       Copy
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

obj%firstCall = obj2%firstCall
obj%isInit = obj2%isInit
obj%nsd = obj2%nsd
obj%order = obj2%order
obj%isIsotropicOrder = obj2%isIsotropicOrder
obj%isAnisotropicOrder = obj2%isAnisotropicOrder
obj%isIpType = obj2%isIpType
obj%isEdgeOrder = obj2%isEdgeOrder
obj%isFaceOrder = obj2%isFaceOrder
obj%isCellOrder = obj2%isCellOrder
obj%isEdgeOrient = obj2%isEdgeOrient
obj%isFaceOrient = obj2%isFaceOrient
obj%isCellOrient = obj2%isCellOrient
obj%tdof = obj2%tdof
obj%nsd = obj2%nsd
obj%xidim = obj2%xidim

obj%topoType = obj2%topoType
obj%elemType = obj2%elemType
obj%elemIndx = obj2%elemIndx
obj%feType = obj2%feType

obj%tEdgeOrder = obj2%tEdgeOrder
obj%tFaceOrder = obj2%tFaceOrder
obj%tCellOrder = obj2%tCellOrder

obj%transformType = obj2%transformType

obj%ipType = obj2%ipType

obj%order = obj2%order
obj%anisoOrder = obj2%anisoOrder

obj%edgeOrder = obj2%edgeOrder
obj%edgeOrient = obj2%edgeOrient

obj%faceOrder = obj2%faceOrder
obj%faceOrient = obj2%faceOrient

obj%cellOrder = obj2%cellOrder
obj%cellOrient = obj2%cellOrient

obj%dofType = obj2%dofType
obj%basisType = obj2%basisType
obj%alpha = obj2%alpha
obj%beta = obj2%beta
obj%lambda = obj2%lambda
obj%refelemDomain = obj2%refelemDomain
obj%baseContinuity = obj2%baseContinuity
obj%baseInterpolation = obj2%baseInterpolation
obj%refelemCoord = obj2%refelemCoord
obj%basisType_char = obj2%basisType_char
obj%ipType_char = obj2%ipType_char
obj%feType_char = obj2%feType_char
CALL obj%quadOpt%Copy(obj2%quadOpt)

isok = ALLOCATED(obj2%coeff)
IF (isok) obj%coeff = obj2%coeff

isok = ALLOCATED(obj2%xx)
IF (isok) obj%xx = obj2%xx

isok = ALLOCATED(obj2%xij)
IF (isok) obj%xij = obj2%xij

isok = ALLOCATED(obj2%temp)
IF (isok) obj%temp = obj2%temp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%Copy(TypeBasisOpt)
IF (ALLOCATED(obj%coeff)) DEALLOCATE (obj%coeff)
IF (ALLOCATED(obj%xx)) DEALLOCATE (obj%xx)
IF (ALLOCATED(obj%xij)) DEALLOCATE (obj%xij)
IF (ALLOCATED(obj%temp)) DEALLOCATE (obj%temp)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Ptr_Vector()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
